const std = @import("std");
const token = @import("token.zig");
const cstring = @cImport(@cInclude("ctype.h"));

const File = std.fs.File;
const TokenType = token.TokenType;
const Token = token.Token;
const Tokenizer = token.Tokenizer;

const LexerError = Tokenizer.Error || File.ReadError || error{FILE_EOF};

pub const Lexer = struct {
    in: File, // program file
    ri: u1 = 0, // current read index
    rb: [3:0]u8 = undefined, // read buffer
    tknzr: *Tokenizer,

    const Self = @This();

    pub fn next_token(self: *Self) LexerError!*Token {
        const c = self.rb[self.ri];

        const tok = switch (c) {
            '=' => try self.tknzr.get(TokenType.ASSIGN, "="),
            ';' => try self.tknzr.get(TokenType.SEMICOLON, ";"),
            '(' => try self.tknzr.get(TokenType.LPAREN, "("),
            ')' => try self.tknzr.get(TokenType.RPAREN, ")"),
            ',' => try self.tknzr.get(TokenType.COMMA, ","),
            '+' => try self.tknzr.get(TokenType.PLUS, "+"),
            '{' => try self.tknzr.get(TokenType.LBRACE, "{"),
            '}' => try self.tknzr.get(TokenType.RBRACE, "}"),
            '\x00' => try self.tknzr.get(TokenType.EOF, "\x00"),
            else => {
                return try self.tknzr.get(TokenType.ILLEGAL, &([1]u8{c}));
            },
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
        self.rb[0] = self.rb[2];
        const r_size = try self.in.read(self.rb[1..]);
        if (r_size == 0) return LexerError.FILE_EOF;
    }
};

pub fn lexer(in: File, tknzr: *Tokenizer) LexerError!Lexer {
    var lxr = Lexer{ .in = in, .tknzr = tknzr };
    _ = try in.read(&lxr.rb);
    return lxr;
}

fn is_letter(c: u8) bool {
    return cstring.isalnum(c) == 1;
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

    var tknzr = Tokenizer.init(std.testing.allocator);
    defer tknzr.deinit();
    var lxr = try lexer(input_f, &tknzr);

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
