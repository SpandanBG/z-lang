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

pub const Parser = struct {
    lxr: *Lexer,
    program: *ast.Program,
    a: Allocator,
    cur_tkn: *Token = undefined,
    peek_tkn: *Token = undefined,

    const Self = @This();

    pub const Error = Lexer.Error || Allocator.Error || error{};

    pub fn init(lxr: *Lexer, a: Allocator) Error!Self {
        var prog = try a.create(ast.Program);
        prog.stmts = AL(*ast.Statement).init(a);

        var prs = Self{ .lxr = lxr, .a = a, .program = prog };

        try prs.next_token();
        try prs.next_token();

        return prs;
    }

    pub fn parse_program(self: *Self) Error!*Program {
        while (self.cur_tkn.t_type != TokenType.EOF) : (_ = try self.next_token()) {
            const stmt = try self.parse_statement() orelse continue;
            try self.program.stmts.append(stmt);
        }

        return self.program;
    }

    pub fn deinit(self: *Self) void {
        for (self.program.stmts.items) |stmt| self.a.destroy(stmt);
        self.program.stmts.deinit();
        self.a.destroy(self.program);
    }

    fn next_token(self: *Self) Error!void {
        self.cur_tkn = self.peek_tkn;
        self.peek_tkn = try self.lxr.next_token();
    }

    fn expect_peek(self: *Self, t_type: TokenType) bool {
        if (self.peek_tkn.t_type != t_type) return false;
        self.next_token() catch return false;
        return true;
    }

    fn parse_statement(self: *Self) Error!?*ast.Statement {
        const stmt = try self.a.create(ast.Statement);

        switch (self.cur_tkn.t_type) {
            TokenType.LET => {
                const let = try self.parse_let_stmt() orelse return null;
                stmt.* = ast.Statement{ .let = let };
            },
            else => return null,
        }

        return stmt;
    }

    fn parse_let_stmt(self: *Self) Error!?ast.Let {
        var let = ast.Let{ .tkn = self.cur_tkn, .name = undefined, .value = undefined };
        if (!self.expect_peek(TokenType.IDENT)) return null;

        let.name = ast.Identifier{ .tkn = self.cur_tkn, .value = self.cur_tkn.literal };
        if (!self.expect_peek(TokenType.ASSIGN)) return null;

        // TODO: skipping expression till we reach ; or EOF
        while (self.cur_tkn.t_type != TokenType.SEMICOLON) : (_ = try self.next_token()) {}

        return let;
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

    try isTrue(memEq(u8, "x", prgm.stmts.items[0].let.name.value));
    try isTrue(memEq(u8, "y", prgm.stmts.items[1].let.name.value));
    try isTrue(memEq(u8, "foobar", prgm.stmts.items[2].let.name.value));
}
