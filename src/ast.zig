const std = @import("std");
const token = @import("token.zig");

const Allocator = std.mem.Allocator;
const AL = std.ArrayList;
const Token = token.Token;

pub const Node = union(enum) {
    stmt: *Statement,
    expr: *Expression,
    prgm: *Program,
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
    let: Let,
};

pub const Expression = union(enum) {
    identifier: Identifier,
};

pub const Program = struct {
    stmts: AL(*Statement),
};

pub const Let = struct { tkn: *Token, name: Identifier, value: Expression };

pub const Identifier = struct { tkn: *Token, value: []u8 };
