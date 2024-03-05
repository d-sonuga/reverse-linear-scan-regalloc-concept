use std::collections::HashMap;
use std::fmt;

fn main() {
    let (input, no_of_regs) = get_input();
    let instns = parse_input(input);
    let mut allocator = Allocator::new(no_of_regs);
    allocator.run(&instns);
    allocator.print_results(&instns);
}

struct Allocator {
    /// Registers available for allocation
    freeregs: Vec<PReg>,
    /// The current locations of all virtual registers that have been encountered so far
    currloc: HashMap<VReg, Location>,
    /// Mappings from virtual to physical registers from the last instruction to the first,
    /// specifically for each input instruction
    allocation: Vec<HashMap<VReg, PReg>>,
    /// A least recently used cache used for eviction
    lru: Lru,
    /// Available slots on the stack
    stackslots: Vec<usize>,
    /// Stack movements to be inserted for spills
    stackmoves: Vec<(usize, StackMove)>,
}

impl Allocator {
    fn new(no_of_regs: i32) -> Self {
        Self {
            freeregs: Self::init_freeregs(no_of_regs),
            currloc: HashMap::new(),
            allocation: vec![],
            lru: Lru::new(no_of_regs as usize),
            stackslots: (0..20).rev().into_iter().collect(),
            stackmoves: vec![],
        }
    }

    /// Main allocation loop
    ///
    /// Visits instructions from the last to the first and drives allocation
    fn run(&mut self, instns: &Vec<Instn>) {
        for (lineno, instn) in instns.iter().enumerate().rev() {
            self.allocation.push(HashMap::new());
            for vreg in instn.def_operands() {
                if let Some(preg) = self.assigned_reg(vreg) {
                    self.currline_allocation().insert(vreg, preg);
                } else {
                    self.allocreg(vreg, lineno);
                }
                self.freealloc(vreg);
            }

            let mut regs_used_in_curr_instn = vec![];
            for vreg in instn.use_operands() {
                let stackloc = self.currloc.get(&vreg).map(|&loc| loc);
                if let Some(preg) = self.assigned_reg(vreg) {
                    self.currline_allocation().insert(vreg, preg);
                    self.lru.poke(vreg, preg);
                } else {
                    let allocated_preg = self.allocreg(vreg, lineno);
                    if regs_used_in_curr_instn.contains(&allocated_preg) {
                        fail("There aren't enough registers for the allocation");
                    }
                    regs_used_in_curr_instn.push(allocated_preg);
                }
                if let Some(Location::Stack(slot)) = stackloc {
                    self.insert_save_to_stack_instn(lineno, vreg, slot);
                    self.stackslots.push(slot / 4)
                }
            }
        }
    }

    /// Initializes the free physical registers list
    fn init_freeregs(no_of_regs: i32) -> Vec<PReg> {
        (0..no_of_regs).into_iter().map(|n| PReg(n)).collect()
    }

    /// Returns the physical register allocated to `preg`
    fn assigned_reg(&mut self, vreg: VReg) -> Option<PReg> {
        if let Some(loc) = self.currloc.get(&vreg) {
            if let Location::Reg(preg) = loc {
                Some(*preg)
            } else {
                None
            }
        } else {
            self.currloc.insert(vreg, Location::Unassigned);
            None
        }
    }

    /// Allocates a physical register to `vreg`
    fn allocreg(&mut self, vreg: VReg, lineno: usize) -> PReg {
        let preg: PReg;
        if !self.freeregs.is_empty() {
            preg = self.freeregs.pop().unwrap();
        } else {
            preg = self.evictreg(lineno);
        }
        self.lru.poke(vreg, preg);
        self.currloc.insert(vreg, Location::Reg(preg));
        self.currline_allocation().insert(vreg, preg);
        preg
    }

    /// Marks the phsyical register holding `vreg` as free
    fn freealloc(&mut self, vreg: VReg) {
        if let Location::Reg(preg) = self.currloc[&vreg] {
            self.currloc.insert(vreg, Location::Unassigned);
            self.freeregs.push(preg);
            self.lru.remove_vreg(preg);
        } else {
            panic!("Attempted to free an unallocated register");
        }
    }

    /// Spills a virtual register to the stack to free up a
    /// physical one for immediate use
    fn evictreg(&mut self, lineno: usize) -> PReg {
        let (vreg, preg) = self.lru.pop();
        let vreg = vreg.unwrap();
        let spillslot = self.new_stack_location();
        self.insert_load_from_stack_instn(lineno, preg, spillslot);
        self.currloc.insert(vreg, spillslot);
        preg
    }

    /// Gets a new stack location for spilling
    fn new_stack_location(&mut self) -> Location {
        if self.stackslots.is_empty() {
            panic!("No more stack memory for spills");
        }
        Location::Stack(self.stackslots.pop().unwrap() * 4)
    }

    /// Specifies that an instruction to save the register holding `vreg`
    /// to `stackloc` should be inserted before `lineno`
    fn insert_save_to_stack_instn(&mut self, lineno: usize, vreg: VReg, stackloc: usize) {
        self.stackmoves.push((
            lineno,
            StackMove {
                reg: if let Location::Reg(preg) = self.currloc[&vreg] {
                    preg
                } else {
                    panic!("The vreg is not in a physical register")
                },
                stackloc,
                tostack: true,
            },
        ))
    }

    /// Specifies that an instruction to load `reg` from `stackloc` should be inserted
    /// after `lineno`
    fn insert_load_from_stack_instn(&mut self, lineno: usize, reg: PReg, stackloc: Location) {
        self.stackmoves.push((
            lineno,
            StackMove {
                reg,
                stackloc: if let Location::Stack(slot) = stackloc {
                    slot
                } else {
                    panic!("stacklock is not a stack location")
                },
                tostack: false,
            },
        ))
    }

    /// The allocation being done for the current instruction
    fn currline_allocation(&mut self) -> &mut HashMap<VReg, PReg> {
        let len = self.allocation.len();
        &mut self.allocation[len - 1]
    }
}

impl Allocator {
    fn print_results(&mut self, instns: &Vec<Instn>) {
        let mut inserted_stackmove = self.stackmoves.pop();
        for (lineno, instn) in instns.iter().enumerate() {
            if let Some((atline, stackmove)) = inserted_stackmove {
                if atline == lineno && stackmove.tostack {
                    println!("save stack+{} {}", stackmove.stackloc, stackmove.reg);
                    inserted_stackmove = self.stackmoves.pop();
                }
            }
            let currlinealloc = self.allocation.pop().unwrap();
            match instn {
                Instn::Store(vreg, constant) => {
                    let preg = currlinealloc[&vreg];
                    println!("store {} {}", preg, constant);
                }
                Instn::BinOp { op, dest, uses } => {
                    println!(
                        "{} {} {} {}",
                        op, currlinealloc[dest], currlinealloc[&uses[0]], currlinealloc[&uses[1]]
                    );
                }
            }
            if let Some((atline, stackmove)) = inserted_stackmove {
                if atline == lineno && !stackmove.tostack {
                    println!("load {} stack+{}", stackmove.reg, stackmove.stackloc);
                    inserted_stackmove = self.stackmoves.pop();
                }
            }
        }
    }
}

/// A least recently used cache organized as a linked list based on a vector
struct Lru {
    /// The list of node information
    ///
    /// Each node corresponds to a physical register.
    /// The index of a node is the `address` from the perspective of the linked list.
    data: Vec<LruNode>,
    /// Index of the most recently used register
    head: usize,
}

#[derive(Clone, Copy)]
struct LruNode {
    /// The previous physical register in the list
    prev: usize,
    /// The next physical register in the list
    next: usize,
    /// The virtual register that is in this physical one
    vreg: Option<VReg>,
}

impl Lru {
    fn new(no_of_regs: usize) -> Self {
        let mut lru = Self {
            head: 0,
            data: vec![
                LruNode {
                    prev: 0,
                    next: 0,
                    vreg: None
                };
                no_of_regs
            ],
        };
        for i in 0..no_of_regs {
            lru.data[i].prev = i.checked_sub(1).unwrap_or(no_of_regs - 1);
            lru.data[i].next = (i + 1) % no_of_regs;
        }
        lru
    }

    /// Marks the physical register `i` as the most recently used
    /// and sets `vreg` as the virtual register it contains
    fn poke(&mut self, vreg: VReg, i: PReg) {
        let i: usize = i.0.try_into().unwrap();
        self.data[i].vreg = Some(vreg);
        let prev_newest = self.head;
        if i == prev_newest {
            return;
        }
        if self.data[prev_newest].prev != i {
            self.remove(i);
            self.insert_before(i, self.head);
        }
        self.head = i;
    }

    /// Gets the least recently used physical register and the virtual
    /// register it contains
    fn pop(&mut self) -> (Option<VReg>, PReg) {
        let oldest = self.data[self.head].prev;
        (self.data[oldest].vreg, PReg(oldest as i32))
    }

    /// Splices out a node from the list
    fn remove(&mut self, i: usize) {
        let (iprev, inext) = (self.data[i].prev, self.data[i].next);
        self.data[iprev].next = self.data[i].next;
        self.data[inext].prev = self.data[i].prev;
    }

    /// Insert node `i` before node `j` in the list
    fn insert_before(&mut self, i: usize, j: usize) {
        let prev = self.data[j].prev;
        self.data[prev].next = i;
        self.data[j].prev = i;
        self.data[i] = LruNode {
            next: j,
            prev,
            vreg: self.data[i].vreg,
        };
    }

    /// Mark `preg` as not containing any physical register
    fn remove_vreg(&mut self, preg: PReg) {
        self.data[preg.0 as usize].vreg = None;
    }
}

/// A physical register
///
/// They are numbered from 0 to `n`-1, where `n` is the number of
/// physical registers
#[derive(Clone, Copy, PartialEq)]
struct PReg(i32);

impl fmt::Display for PReg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "r{}", self.0)
    }
}

/// A virtual register
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct VReg(u32);

/// A virtual register's location
#[derive(Clone, Copy, PartialEq)]
enum Location {
    /// A physical register
    Reg(PReg),
    /// An offset on the stack
    Stack(usize),
    /// Not yet assigned
    Unassigned,
}

impl From<&str> for VReg {
    fn from(s: &str) -> Self {
        if &s[0..1] != "v" || s.len() != 2 {
            fail("Failed to parse virtual register {s}");
        }
        VReg(
            s.chars()
                .skip(1)
                .next()
                .unwrap()
                .to_digit(10)
                .unwrap_or_else(|| {
                    fail("Failed to parse virtual register {s}");
                }),
        )
    }
}

/// Represents an instruction to be inserted to handle a spill
/// or restoration of a spill
#[derive(Clone, Copy)]
struct StackMove {
    /// The physical register involved
    reg: PReg,
    /// The location/offset on the stack
    stackloc: usize,
    /// If true, the value is moving from the register to the stack
    tostack: bool,
}

/// An instruction in the input
#[derive(Clone, PartialEq)]
enum Instn {
    /// The constant and the virtual register it's to be stored in
    Store(VReg, i32),
    BinOp {
        op: String,
        dest: VReg,
        uses: [VReg; 2],
    },
}

impl Instn {
    /// The virtual registers that were written to in the instruction
    fn def_operands(&self) -> Vec<VReg> {
        match *self {
            Self::Store(vreg, _) => vec![vreg],
            Self::BinOp { dest, .. } => vec![dest],
        }
    }

    /// The virtual registers that were read in the instruction
    fn use_operands(&self) -> Vec<VReg> {
        match *self {
            Self::Store(_, _) => vec![],
            Self::BinOp { uses, .. } => uses.into(),
        }
    }
}

fn parse_input(input: String) -> Vec<Instn> {
    let mut instns = vec![];
    for (lineno, line) in input.split("\n").enumerate() {
        let line: Vec<&str> = line.split(" ").collect();
        if line.len() != 3 && line.len() != 4 {
            fail(&format!("Incorrect instruction format at line {lineno}"));
        }
        match line[0] {
            "store" => instns.push(Instn::Store(
                VReg::from(line[1]),
                line[2].parse().unwrap_or_else(|_| {
                    fail(&format!(
                        "At {lineno}, the store instruction expects a 32-bit signed integer"
                    ))
                }),
            )),
            op @ ("add" | "sub") => instns.push(Instn::BinOp {
                op: op.into(),
                dest: VReg::from(line[1]),
                uses: [VReg::from(line[2]), VReg::from(line[3])],
            }),
            _ => fail("Input should contain only add, sub and store instructions"),
        }
    }
    instns
}

fn get_input() -> (String, i32) {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 3 {
        fail("USAGE: prog [input filepath] [num of physical registers]");
    }
    let filepath = args[1].clone();
    (
        std::fs::read_to_string(filepath)
            .unwrap_or_else(|_| {
                fail("Failed to read the file");
            })
            .trim()
            .into(),
        args[2].parse().unwrap_or_else(|_| {
            fail("Failed to parse the number of registers");
        }),
    )
}

fn fail(msg: &str) -> ! {
    eprintln!("{msg}");
    std::process::exit(1);
}
