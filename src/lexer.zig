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
    ri: u2 = 0, // current read index
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
            '=' => eq_blk: {
                if (self.peek_char() == '=') {
                    try self.read_char();
                    break :eq_blk try self.tknzr.get(TokenType.EQ, "==");
                }
                break :eq_blk try self.tknzr.get(TokenType.ASSIGN, "=");
            },
            '>' => gt_blk: {
                if (self.peek_char() == '=') {
                    try self.read_char();
                    break :gt_blk try self.tknzr.get(TokenType.GTE, ">=");
                }
                break :gt_blk try self.tknzr.get(TokenType.GT, ">");
            },
            '<' => lt_blk: {
                if (self.peek_char() == '=') {
                    try self.read_char();
                    break :lt_blk try self.tknzr.get(TokenType.LTE, "<=");
                }
                break :lt_blk try self.tknzr.get(TokenType.LT, "<");
            },
            '!' => bang_vlk: {
                if (self.peek_char() == '=') {
                    try self.read_char();
                    break :bang_vlk try self.tknzr.get(TokenType.NEQ, "!=");
                }
                break :bang_vlk try self.tknzr.get(TokenType.BANG, "!");
            },
            ';' => try self.tknzr.get(TokenType.SEMICOLON, ";"),
            '-' => try self.tknzr.get(TokenType.MINUS, "-"),
            '/' => try self.tknzr.get(TokenType.SLASH, "/"),
            '*' => try self.tknzr.get(TokenType.ASTERISK, "*"),
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

                if (is_number(c)) {
                    self.flex_buf.clearRetainingCapacity();
                    while (is_number(self.rb[self.ri])) : (_ = try self.read_char()) {
                        try self.flex_buf.append(self.rb[self.ri]);
                    }
                    const l = self.flex_buf.items[0..self.flex_buf.items.len];

                    return try self.tknzr.get(TokenType.INT, l);
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
        const r_size = try self.in.read(self.rb[1..3]);
        if (r_size <= 1) self.rb[2] = '\x00';
        if (r_size == 0) self.rb[1] = '\x00';
    }

    fn peek_char(self: Self) u8 {
        return self.rb[self.ri + 1];
    }

    fn skip_whitespace(self: *Self) Error!void {
        while (is_space(self.rb[self.ri])) : (_ = try self.read_char()) {}
    }
};

fn is_letter(c: u8) bool {
    return cstring.isalpha(c) != 0;
}

fn is_number(c: u8) bool {
    return cstring.isdigit(c) != 0;
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
        .{ .e_ttype = TokenType.INT, .e_literal = "5" },
        .{ .e_ttype = TokenType.SEMICOLON, .e_literal = ";" },
        .{ .e_ttype = TokenType.LET, .e_literal = "let" },
        .{ .e_ttype = TokenType.IDENT, .e_literal = "ten" },
        .{ .e_ttype = TokenType.ASSIGN, .e_literal = "=" },
        .{ .e_ttype = TokenType.INT, .e_literal = "10" },
        .{ .e_ttype = TokenType.SEMICOLON, .e_literal = ";" },
        .{ .e_ttype = TokenType.LET, .e_literal = "let" },
        .{ .e_ttype = TokenType.IDENT, .e_literal = "add" },
        .{ .e_ttype = TokenType.ASSIGN, .e_literal = "=" },
        .{ .e_ttype = TokenType.FUNCTION, .e_literal = "fn" },
        .{ .e_ttype = TokenType.LPAREN, .e_literal = "(" },
        .{ .e_ttype = TokenType.IDENT, .e_literal = "x" },
        .{ .e_ttype = TokenType.COMMA, .e_literal = "," },
        .{ .e_ttype = TokenType.IDENT, .e_literal = "y" },
        .{ .e_ttype = TokenType.RPAREN, .e_literal = ")" },
        .{ .e_ttype = TokenType.LBRACE, .e_literal = "{" },
        .{ .e_ttype = TokenType.IDENT, .e_literal = "x" },
        .{ .e_ttype = TokenType.PLUS, .e_literal = "+" },
        .{ .e_ttype = TokenType.IDENT, .e_literal = "y" },
        .{ .e_ttype = TokenType.SEMICOLON, .e_literal = ";" },
        .{ .e_ttype = TokenType.RBRACE, .e_literal = "}" },
        .{ .e_ttype = TokenType.SEMICOLON, .e_literal = ";" },
        .{ .e_ttype = TokenType.LET, .e_literal = "let" },
        .{ .e_ttype = TokenType.IDENT, .e_literal = "result" },
        .{ .e_ttype = TokenType.ASSIGN, .e_literal = "=" },
        .{ .e_ttype = TokenType.IDENT, .e_literal = "add" },
        .{ .e_ttype = TokenType.LPAREN, .e_literal = "(" },
        .{ .e_ttype = TokenType.IDENT, .e_literal = "five" },
        .{ .e_ttype = TokenType.COMMA, .e_literal = "," },
        .{ .e_ttype = TokenType.IDENT, .e_literal = "ten" },
        .{ .e_ttype = TokenType.RPAREN, .e_literal = ")" },
        .{ .e_ttype = TokenType.SEMICOLON, .e_literal = ";" },
        .{ .e_ttype = TokenType.BANG, .e_literal = "!" },
        .{ .e_ttype = TokenType.MINUS, .e_literal = "-" },
        .{ .e_ttype = TokenType.SLASH, .e_literal = "/" },
        .{ .e_ttype = TokenType.ASTERISK, .e_literal = "*" },
        .{ .e_ttype = TokenType.INT, .e_literal = "5" },
        .{ .e_ttype = TokenType.SEMICOLON, .e_literal = ";" },
        .{ .e_ttype = TokenType.INT, .e_literal = "5" },
        .{ .e_ttype = TokenType.LT, .e_literal = "<" },
        .{ .e_ttype = TokenType.INT, .e_literal = "10" },
        .{ .e_ttype = TokenType.GT, .e_literal = ">" },
        .{ .e_ttype = TokenType.INT, .e_literal = "5" },
        .{ .e_ttype = TokenType.SEMICOLON, .e_literal = ";" },
        .{ .e_ttype = TokenType.INT, .e_literal = "5" },
        .{ .e_ttype = TokenType.LTE, .e_literal = "<=" },
        .{ .e_ttype = TokenType.INT, .e_literal = "10" },
        .{ .e_ttype = TokenType.GTE, .e_literal = ">=" },
        .{ .e_ttype = TokenType.INT, .e_literal = "5" },
        .{ .e_ttype = TokenType.SEMICOLON, .e_literal = ";" },
        .{ .e_ttype = TokenType.INT, .e_literal = "5" },
        .{ .e_ttype = TokenType.NEQ, .e_literal = "!=" },
        .{ .e_ttype = TokenType.INT, .e_literal = "5" },
        .{ .e_ttype = TokenType.SEMICOLON, .e_literal = ";" },
        .{ .e_ttype = TokenType.INT, .e_literal = "10" },
        .{ .e_ttype = TokenType.EQ, .e_literal = "==" },
        .{ .e_ttype = TokenType.INT, .e_literal = "10" },
        .{ .e_ttype = TokenType.SEMICOLON, .e_literal = ";" },
        .{ .e_ttype = TokenType.IF, .e_literal = "if" },
        .{ .e_ttype = TokenType.LPAREN, .e_literal = "(" },
        .{ .e_ttype = TokenType.INT, .e_literal = "5" },
        .{ .e_ttype = TokenType.LT, .e_literal = "<" },
        .{ .e_ttype = TokenType.INT, .e_literal = "10" },
        .{ .e_ttype = TokenType.RPAREN, .e_literal = ")" },
        .{ .e_ttype = TokenType.LBRACE, .e_literal = "{" },
        .{ .e_ttype = TokenType.RETURN, .e_literal = "return" },
        .{ .e_ttype = TokenType.TRUE, .e_literal = "true" },
        .{ .e_ttype = TokenType.SEMICOLON, .e_literal = ";" },
        .{ .e_ttype = TokenType.RBRACE, .e_literal = "}" },
        .{ .e_ttype = TokenType.ELSE, .e_literal = "else" },
        .{ .e_ttype = TokenType.LBRACE, .e_literal = "{" },
        .{ .e_ttype = TokenType.RETURN, .e_literal = "return" },
        .{ .e_ttype = TokenType.FALSE, .e_literal = "false" },
        .{ .e_ttype = TokenType.SEMICOLON, .e_literal = ";" },
        .{ .e_ttype = TokenType.RBRACE, .e_literal = "}" },
        .{ .e_ttype = TokenType.EOF, .e_literal = "\x00" },
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
