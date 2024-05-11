const std = @import("std");

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
    literal: [:0]const u8,
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

// -------------------- TEST

test "debug str" {
    try std.testing.expect(std.mem.eql(u8, "EOF", debug_str(TokenType.EOF)));
}
