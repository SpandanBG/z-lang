const std = @import("std");

const Allocator = std.mem.Allocator;
const SHMap = std.StringHashMap;

pub const TokenType = enum(u8) {
    ILLEGAL,
    EOF,

    // Identifiers + literals
    IDENT,
    INT,

    // Operators
    ASSIGN,
    PLUS,

    // Delimiters
    COMMA,
    SEMICOLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    // Keywords
    FUNCTION,
    LET,
};

pub const Token = struct {
    t_type: TokenType,
    literal: []u8,
};

pub fn debug_str(t_type: TokenType) [:0]const u8 {
    return switch (t_type) {
        .ILLEGAL => "ILLEGAL",
        .EOF => "EOF",

        .IDENT => "IDENT",
        .INT => "INT",

        .ASSIGN => "ASSIGN",
        .PLUS => "PLUS",

        .COMMA => "COMMA",
        .SEMICOLON => "SEMICOLON",

        .LPAREN => "LPAREN",
        .RPAREN => "RPAREN",
        .LBRACE => "LBRACE",
        .RBRACE => "RBRACE",

        .FUNCTION => "FUNCTION",
        .LET => "LET",
    };
}

const Tokenizer = struct {
    tm: SHMap(*Token), // token memo
    a: Allocator,

    const Self = @This();

    const Error = Allocator.Error || error{};

    pub fn init(a: Allocator) Self {
        const tm = SHMap(*Token).init(a);
        return .{ .tm = tm, .a = a };
    }

    pub fn get(self: *Self, t_type: TokenType, literal: []const u8) Error!*Token {
        if (self.tm.get(literal)) |t| return t;
        const t = try self.new_token(t_type, literal);
        try self.tm.put(literal, t);
        return t;
    }

    pub fn deinit(self: *Self) void {
        var iter = self.tm.iterator();
        while (iter.next()) |t| {
            const v = t.value_ptr;
            self.a.free(v.*.literal);
            self.a.destroy(v.*);
        }
        self.tm.deinit();
    }

    fn new_token(self: Self, t_type: TokenType, literal: []const u8) Error!*Token {
        var t = try self.a.create(Token);
        t.t_type = t_type;
        t.literal = try self.a.alloc(u8, literal.len);
        @memcpy(t.literal, literal);
        return t;
    }
};

// -------------------- TEST

const isEq = std.testing.expectEqual;
const isTrue = std.testing.expect;
const memEq = std.mem.eql;

test "debug str" {
    try std.testing.expect(std.mem.eql(u8, "EOF", debug_str(TokenType.EOF)));
}

test "tokenizer" {
    var tknz = Tokenizer.init(std.testing.allocator);
    defer tknz.deinit();

    const t = try tknz.get(TokenType.LET, "let");
    const t2 = try tknz.get(TokenType.LET, "let");

    isEq(@intFromEnum(TokenType.LET), @intFromEnum(t.t_type)) catch |err| {
        std.log.err(
            "\nexpected t_type = {s}\nfound t_type = {s}\n",
            .{ debug_str(TokenType.LET), debug_str(t.t_type) },
        );
        return err;
    };
    try isTrue(memEq(u8, "let", t.literal));
    try isEq(@intFromPtr(t), @intFromPtr(t2));
}
