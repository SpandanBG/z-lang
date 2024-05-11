const std = @import("std");
const token = @import("token.zig");
const cstring = @cImport(@cInclude("ctype.h"));

const Allocator = std.mem.Allocator;
const File = std.fs.File;
const AList = std.ArrayList;

const TokenType = token.TokenType;
const Token = token.Token;
const Tokenizer = token.Tokenizer;

pub const Lexer = struct {
    in: File, // program file
    ri: u1 = 0, // current read index
    rb: [3:0]u8 = undefined, // read buffer
    a: Allocator,
    flex_buf: AList(u8),
    tknzr: *Tokenizer,

    pub const Error = Tokenizer.Error || Allocator.Error || File.ReadError || error{FILE_EOF};

    const Self = @This();

    fn init(in: File, tknzr: *Tokenizer, a: Allocator) Error!Self {
        const fb = AList(u8).init(a);
        var lxr = Lexer{ .in = in, .flex_buf = fb, .tknzr = tknzr, .a = a };
        _ = try in.read(&lxr.rb);
        return lxr;
    }

    pub fn next_token(self: *Self) Error!*Token {
        try self.skip_whitespace();

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
                if (is_letter(c)) {
                    self.flex_buf.clearRetainingCapacity();

                    while (is_letter(self.rb[self.ri])) : (_ = try self.read_char()) {
                        try self.flex_buf.append(self.rb[self.ri]);
                    }
                    const l = self.flex_buf.items[0..self.flex_buf.items.len];
                    const t = self.tknzr.get_type(l);

                    return try self.tknzr.get(t, l);
                }

                return try self.tknzr.get(TokenType.ILLEGAL, &([1]u8{c}));
            },
        };

        try self.read_char();
        return tok;
    }

    pub fn deinit(self: *Self) void {
        self.flex_buf.deinit();
    }

    fn read_char(self: *Self) Error!void {
        if (self.ri == 0) {
            self.ri += 1;
            return;
        }

        self.ri = 0;
        self.rb[0] = self.rb[2];
        const r_size = try self.in.read(self.rb[1..]);
        if (r_size == 0) return Error.FILE_EOF;
    }

    fn skip_whitespace(self: *Self) Error!void {
        while (is_space(self.rb[self.ri])) : (_ = try self.read_char()) {}
    }
};

fn is_letter(c: u8) bool {
    return cstring.isalpha(c) != 0;
}

fn is_space(c: u8) bool {
    return cstring.isspace(c) != 0;
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
        .{ .e_ttype = TokenType.LET, .e_literal = "let" },
        .{ .e_ttype = TokenType.IDENT, .e_literal = "five" },
        .{ .e_ttype = TokenType.ASSIGN, .e_literal = "=" },
        // .{ .e_ttype = TokenType.INT, .e_literal = "5" },
    };

    var tknzr = try Tokenizer.init(std.testing.allocator);
    defer tknzr.deinit();

    var lxr = try Lexer.init(input_f, &tknzr, std.testing.allocator);
    defer lxr.deinit();

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
