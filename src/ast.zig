const std = @import("std");
const token = @import("token.zig");

const Allocator = std.mem.Allocator;
const AL = std.ArrayList;
const Token = token.Token;

pub const Node = union(enum) {
    STMT: *Statement,
    EXPR: *Expression,
    PRGM: *Program,
};

pub fn destroy_node(node: *Node, allocator: Allocator) void {
    defer allocator.destroy(node);
    switch (node) {
        .stmt => |s| allocator.destroy(s, allocator),
        .expr => |e| allocator.destroy(e, allocator),
        .prgm => |p| allocator.destroy(p, allocator),
    }
}

pub const Statement = union(enum) {
    LET: Let,
    RETURN: Return,
};

pub const Expression = union(enum) {
    IDENTIFIER: Identifier,
};

pub const Program = struct {
    STMTS: AL(*Statement),
};

pub const Let = struct { tkn: *Token, name: Identifier, value: Expression };

pub const Identifier = struct { tkn: *Token, value: []const u8 };

pub const Return = struct { tkn: *Token, return_value: Expression };
