const std = @import("std");
const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const token = @import("token.zig");

const AL = std.ArrayList;
const Allocator = std.mem.Allocator;
const Program = ast.Program;
const Lexer = lexer.Lexer;
const Token = token.Token;

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

        _ = try prs.next_token();
        _ = try prs.next_token();

        return prs;
    }

    pub fn parse_program(self: *Self) Error!*Program {
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
};

// ---------------------------- TEST

const isEq = std.testing.expectEqual;

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
