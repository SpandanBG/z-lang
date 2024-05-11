const std = @import("std");

const memEq = std.mem.eql;
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
    MINUS,
    BANG,
    ASTERISK,
    SLASH,

    EQ,
    NEQ,
    LT,
    LTE,
    GT,
    GTE,

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
        .MINUS => "MINUS",
        .BANG => "BANG",
        .ASTERISK => "ASTERISK",
        .SLASH => "SLASH",

        .EQ => "EQ",
        .NEQ => "NEQ",
        .LT => "LT",
        .LTE => "LTE",
        .GT => "GT",
        .GTE => "GTE",

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

pub const Tokenizer = struct {
    tm: SHMap(*Token), // token memo
    km: SHMap(TokenType), // keyword memo
    a: Allocator,

    const Self = @This();

    pub const Error = Allocator.Error || error{};

    pub fn init(a: Allocator) Error!Self {
        const tm = SHMap(*Token).init(a);

        var km = SHMap(TokenType).init(a);
        try km.put("let", TokenType.LET);
        try km.put("fn", TokenType.FUNCTION);

        return .{ .tm = tm, .km = km, .a = a };
    }

    pub fn get(self: *Self, t_type: TokenType, literal: []const u8) Error!*Token {
        if (self.tm.get(literal)) |t| return t;
        const t = try self.new_token(t_type, literal);
        try self.tm.put(t.literal, t);
        return t;
    }

    pub fn get_type(self: *Self, literal: []const u8) TokenType {
        return self.km.get(literal) orelse TokenType.IDENT;
    }

    pub fn deinit(self: *Self) void {
        var iter = self.tm.iterator();
        while (iter.next()) |t| {
            const v = t.value_ptr;
            self.a.free(v.*.literal);
            self.a.destroy(v.*);
        }
        self.tm.deinit();
        self.km.deinit();
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

test "debug str" {
    try std.testing.expect(std.mem.eql(u8, "EOF", debug_str(TokenType.EOF)));
}

test "tokenizer" {
    var tknz = try Tokenizer.init(std.testing.allocator);
    defer tknz.deinit();

    const t = try tknz.get(TokenType.LET, "let");
    const t2 = try tknz.get(TokenType.LET, "let");

    isEq(@intFromEnum(tknz.get_type("let")), @intFromEnum(t.t_type)) catch |err| {
        std.log.err(
            "\nexpected t_type = {s}\nfound t_type = {s}\n",
            .{ debug_str(tknz.get_type("let")), debug_str(t.t_type) },
        );
        return err;
    };
    try isTrue(memEq(u8, "let", t.literal));
    try isEq(@intFromPtr(t), @intFromPtr(t2));
}
