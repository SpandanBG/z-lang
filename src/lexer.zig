const std = @import("std");
const token = @import("token.zig");

const File = std.fs.File;
const TokenType = token.TokenType;
const Token = token.Token;

const LexerError = File.ReadError || error{FILE_EOF};

pub const Lexer = struct {
    in: File, // program file
    ri: u1 = 1, // current read index
    rb: [2:0]u8 = .{ '\x00', '\x00' }, // read buffer

    const Self = @This();

    pub fn next_token(self: *Self) LexerError!Token {
        const tok = switch (self.rb[self.ri]) {
            '=' => new_token(TokenType.ASSIGN, "="),
            ';' => new_token(TokenType.SEMICOLON, ";"),
            '(' => new_token(TokenType.LPAREN, "("),
            ')' => new_token(TokenType.RPAREN, ")"),
            ',' => new_token(TokenType.COMMA, ","),
            '+' => new_token(TokenType.PLUS, "+"),
            '{' => new_token(TokenType.LBRACE, "{"),
            '}' => new_token(TokenType.RBRACE, "}"),
            '\x00' => new_token(TokenType.EOF, "\x00"),
            else => new_token(TokenType.ILLEGAL, ""),
        };

        try self.read_char();
        return tok;
    }

    fn read_char(self: *Self) LexerError!void {
        if (self.ri == 0) {
            self.ri += 1;
            return;
        }

        self.ri = 0;
        const r_size = try self.in.read(&self.rb);
        if (r_size == 0) return LexerError.FILE_EOF;
    }
};

pub fn lexer(in: File) LexerError!Lexer {
    var lxr = Lexer{ .in = in };
    try lxr.read_char();
    return lxr;
}

fn new_token(t_type: TokenType, literal: [:0]const u8) Token {
    return Token{ .t_type = t_type, .literal = literal };
}

// --------------------------------- TEST

const isEq = std.testing.expectEqual;
const isTrue = std.testing.expect;
const memEq = std.mem.eql;

test "next token" {
    const input_f = try std.fs.cwd().openFile("./mocks/lexer_mocks.txt", .{});
    defer input_f.close();

    const tests = [_]struct {
        e_ttype: TokenType,
        e_literal: [:0]const u8,
    }{
        .{ .e_ttype = TokenType.ASSIGN, .e_literal = "=" },
        .{ .e_ttype = TokenType.PLUS, .e_literal = "+" },
        .{ .e_ttype = TokenType.LPAREN, .e_literal = "(" },
        .{ .e_ttype = TokenType.RPAREN, .e_literal = ")" },
        .{ .e_ttype = TokenType.LBRACE, .e_literal = "{" },
        .{ .e_ttype = TokenType.RBRACE, .e_literal = "}" },
        .{ .e_ttype = TokenType.COMMA, .e_literal = "," },
        .{ .e_ttype = TokenType.SEMICOLON, .e_literal = ";" },
    };

    var lxr = try lexer(input_f);

    for (tests) |t| {
        const tok = try lxr.next_token();

        isEq(@intFromEnum(t.e_ttype), @intFromEnum(tok.t_type)) catch |err| {
            std.log.err(
                "\nexpected t_type = {s}\nfound t_type = {s}\n",
                .{ token.debug_str(t.e_ttype), token.debug_str(tok.t_type) },
            );
            return err;
        };
        try isTrue(memEq(u8, t.e_literal, tok.literal));
    }
}
