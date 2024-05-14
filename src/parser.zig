const std = @import("std");
const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const token = @import("token.zig");

const AL = std.ArrayList;
const Allocator = std.mem.Allocator;
const Program = ast.Program;
const Lexer = lexer.Lexer;
const Token = token.Token;
const TokenType = token.TokenType;

const Precedence = enum(u8) {
    LOWEST,
    EQUALS, // ==
    LESSGREATER, // <, >, <=, >=
    SUM, // +
    PRODUCT, // *
    PREFIX, // -X, !X
    CALL, // func()
};

pub const Parser = struct {
    lxr: *Lexer,
    program: *ast.Program,
    errors: AL(*ErrorCtx),
    a: Allocator,
    cur_tkn: *Token = undefined,
    peek_tkn: *Token = undefined,

    const Self = @This();

    pub const Error = Lexer.Error || Allocator.Error || error{
        LET_PARSE_ERR,
        RETURN_PARSE_ERR,
    };
    pub const ErrorCtx = union(enum) {
        PEEK_ERROR: struct { expected: TokenType, actual: TokenType },
    };

    pub fn init(lxr: *Lexer, a: Allocator) Error!Self {
        const errs = AL(*ErrorCtx).init(a);
        var prog = try a.create(ast.Program);
        prog.STMTS = AL(*ast.Statement).init(a);

        var prs = Self{ .lxr = lxr, .program = prog, .errors = errs, .a = a };
        try prs.next_token();
        try prs.next_token();
        return prs;
    }

    pub fn parse_program(self: *Self) Error!*Program {
        while (self.cur_tkn.t_type != TokenType.EOF) : (_ = try self.next_token()) {
            const stmt = try self.parse_statement() orelse continue;
            try self.program.STMTS.append(stmt);
        }

        return self.program;
    }

    pub fn get_errors(self: *Self) []*ErrorCtx {
        return self.errors.items[0..];
    }

    pub fn deinit(self: *Self) void {
        for (self.program.STMTS.items) |stmt| self.a.destroy(stmt);
        self.program.STMTS.deinit();

        for (self.errors.items) |err| self.a.destroy(err);
        self.errors.deinit();

        self.a.destroy(self.program);
    }

    fn next_token(self: *Self) Error!void {
        self.cur_tkn = self.peek_tkn;
        self.peek_tkn = try self.lxr.next_token();
    }

    fn expect_peek(self: *Self, e_type: TokenType) bool {
        const is_expected = self.peek_error(e_type) catch return false;
        if (!is_expected) return false;
        self.next_token() catch return false;
        return true;
    }

    fn peek_error(self: *Self, e_type: TokenType) Error!bool {
        if (self.peek_tkn.t_type == e_type) return true;
        const err_ctx = try self.a.create(ErrorCtx);
        err_ctx.* = ErrorCtx{ .PEEK_ERROR = .{ .expected = e_type, .actual = self.peek_tkn.t_type } };
        try self.errors.append(err_ctx);
        return false;
    }

    fn parse_statement(self: *Self) Error!?*ast.Statement {
        const stmt = try self.a.create(ast.Statement);

        stmt.* = switch (self.cur_tkn.t_type) {
            TokenType.LET => ast.Statement{ .LET = self.parse_let_stmt() catch return null },
            TokenType.RETURN => ast.Statement{ .RETURN = self.parse_return_stmt() catch return null },
            else => ast.Statement{ .EXPR_STMT = self.parse_expr_stmt() catch return null },
        };

        return stmt;
    }

    fn parse_expr(self: *Self, pcd: Precedence) Error!?ast.Expression {
        _ = pcd;

        const left_exp = try self.parse_prefix_expr();
        return left_exp;
    }

    fn parse_expr_stmt(self: *Self) Error!ast.ExpressionStmt {
        var expr_stmt = ast.ExpressionStmt{ .tkn = self.cur_tkn, .expr = undefined };
        expr_stmt.expr = try self.parse_expr(Precedence.LOWEST) orelse undefined;
        if (self.peek_tkn.t_type == TokenType.SEMICOLON) try self.next_token();
        return expr_stmt;
    }

    fn parse_prefix_expr(self: *Self) Error!?ast.Expression {
        return switch (self.cur_tkn.t_type) {
            TokenType.IDENT => |_| try self.parse_identifier(),
            else => return null,
        };
    }

    fn parse_let_stmt(self: *Self) Error!ast.Let {
        var let = ast.Let{ .tkn = self.cur_tkn, .name = undefined, .value = undefined };
        if (!self.expect_peek(TokenType.IDENT)) return Error.LET_PARSE_ERR;

        let.name = ast.Identifier{ .tkn = self.cur_tkn, .value = self.cur_tkn.literal };
        if (!self.expect_peek(TokenType.ASSIGN)) return Error.LET_PARSE_ERR;

        // TODO: skipping expression till we reach ; or EOF
        while (self.cur_tkn.t_type != TokenType.SEMICOLON) : (_ = try self.next_token()) {}

        return let;
    }

    fn parse_return_stmt(self: *Self) Error!ast.Return {
        const rt = ast.Return{ .tkn = self.cur_tkn, .return_value = undefined };
        try self.next_token();

        // TODO: skipping expression till we reach ; or EOF
        while (self.cur_tkn.t_type != TokenType.SEMICOLON) : (_ = try self.next_token()) {}

        return rt;
    }

    fn parse_identifier(self: *Self) Error!ast.Expression {
        return ast.Expression{ .IDENTIFIER = .{ .tkn = self.cur_tkn, .value = self.cur_tkn.literal } };
    }
};

// ---------------------------- TEST

const isEq = std.testing.expectEqual;
const isTrue = std.testing.expect;
const memEq = std.mem.eql;

test "parser init - deinit" {
    var fba = std.io.fixedBufferStream("");
    const allocator = std.testing.allocator;

    var tknz = try token.Tokenizer.init(allocator);
    defer tknz.deinit();

    var lxr = try Lexer.init(fba.reader().any(), &tknz, allocator);
    defer lxr.deinit();

    var prsr = try Parser.init(&lxr, allocator);
    defer prsr.deinit();
}

test "let statement" {
    var fba = std.io.fixedBufferStream(
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 42;
    );
    const allocator = std.testing.allocator;

    var tknz = try token.Tokenizer.init(allocator);
    defer tknz.deinit();

    var lxr = try Lexer.init(fba.reader().any(), &tknz, allocator);
    defer lxr.deinit();

    var prsr = try Parser.init(&lxr, allocator);
    defer prsr.deinit();

    const prgm = try prsr.parse_program();

    try isEq(0, prsr.get_errors().len);
    try isTrue(memEq(u8, "x", prgm.STMTS.items[0].LET.name.value));
    try isTrue(memEq(u8, "y", prgm.STMTS.items[1].LET.name.value));
    try isTrue(memEq(u8, "foobar", prgm.STMTS.items[2].LET.name.value));
}

test "return statement" {
    var fba = std.io.fixedBufferStream(
        \\return 5;
        \\return 10;
        \\return 42;
    );
    const allocator = std.testing.allocator;

    var tknz = try token.Tokenizer.init(allocator);
    defer tknz.deinit();

    var lxr = try Lexer.init(fba.reader().any(), &tknz, allocator);
    defer lxr.deinit();

    var prsr = try Parser.init(&lxr, allocator);
    defer prsr.deinit();

    const prgm = try prsr.parse_program();

    try isEq(0, prsr.get_errors().len);
    for (prgm.STMTS.items) |stmt| try isEq(TokenType.RETURN, stmt.RETURN.tkn.t_type);
}

test "identifier expr" {
    var fba = std.io.fixedBufferStream(
        \\foobar;
    );
    const allocator = std.testing.allocator;

    var tknz = try token.Tokenizer.init(allocator);
    defer tknz.deinit();

    var lxr = try Lexer.init(fba.reader().any(), &tknz, allocator);
    defer lxr.deinit();

    var prsr = try Parser.init(&lxr, allocator);
    defer prsr.deinit();

    const prgm = try prsr.parse_program();

    try isEq(0, prsr.get_errors().len);
    try isTrue(memEq(u8, "foobar", prgm.STMTS.items[0].EXPR_STMT.expr.IDENTIFIER.value));
}
