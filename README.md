# Reverse Linear Scan Allocator Concept

A toy register allocator to implement the algorithm described in https://www.mattkeeter.com/blog/2022-10-04-ssra/.

Accepts a program with only `add`, `sub`, `save`, `load` and `store` commands. `store` can only store a 
constant in a virtual register. `add` and `sub` can only operate on registers. `save` copies a value from a
register to a stack slot and `load` copies a value from a stack slot to a register.
The input is expected not to have any `save` or `load` instructions.
The first operand is the output register. The rest are the input registers.
Registers are assumed to be of the form `vn` where `n` is the register number.
SSA form is assumed.

For example, a sample program with 5 virtual registers:

```
store v0 5
store v1 6
add v2 v0 v1
add v3 v2 v1
sub v4 v0 v3
```

A sample output with only 2 physical registers.:

```
store r0 5
store r1 6
save stack+0 r0
add r0 r0 r1
add r0 r0 r1
load r1 stack+0
sub r1 r1 r0
```

To run the sample input with 2 physical registers:

```
cargo run sample/input 2
```
