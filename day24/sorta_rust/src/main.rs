use itertools::izip;
use rand::distributions::Uniform;
use rand::Rng;
use std::collections::VecDeque;
use std::fmt;
use std::fs;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct State {
    x: i64,
    y: i64,
    z: i64,
    w: i64,
}

impl State {
    fn success(&self) -> bool {
        self.z == 0
    }
}

fn int_to_stack(number: i64) -> VecDeque<i64> {
    let mut result = VecDeque::new();
    let mut n = number;
    while n > 0 {
        result.push_front(n % 26);
        n /= 26;
    }
    result
}

impl fmt::Display for State {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "State {{ x={} y={} w={} z=[{}] }}",
            self.x,
            self.y,
            self.w,
            int_to_stack(self.z)
                .iter()
                .map(i64::to_string)
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

fn init_state() -> State {
    State {
        x: 0,
        y: 0,
        z: 0,
        w: 0,
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Arg {
    X,
    Y,
    Z,
    W,
    Number(i64),
}

impl fmt::Display for Arg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Arg::X => "x".to_string(),
                Arg::Y => "y".to_string(),
                Arg::Z => "z".to_string(),
                Arg::W => "w".to_string(),
                Arg::Number(n) => n.to_string(),
            }
        )
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Instruction {
    Input { dest: Arg },
    Add { dest: Arg, source: Arg },
    Mul { dest: Arg, source: Arg },
    Div { dest: Arg, source: Arg },
    Mod { dest: Arg, source: Arg },
    Eql { dest: Arg, source: Arg },
    Ne { dest: Arg, source: Arg },
    Mov { dest: Arg, source: Arg },
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instruction::Input { dest } => write!(f, "inp {}", dest),
            Instruction::Add { dest, source } => write!(f, "add {} {}", dest, source),
            Instruction::Mul { dest, source } => write!(f, "mul {} {}", dest, source),
            Instruction::Div { dest, source } => write!(f, "div {} {}", dest, source),
            Instruction::Mod { dest, source } => write!(f, "mod {} {}", dest, source),
            Instruction::Eql { dest, source } => write!(f, "eql {} {}", dest, source),
            Instruction::Ne { dest, source } => write!(f, "ne {} {}", dest, source),
            Instruction::Mov { dest, source } => write!(f, "mov {} {}", dest, source),
        }
    }
}

fn parse_arg(s: &str) -> Arg {
    match s {
        "x" => Arg::X,
        "y" => Arg::Y,
        "z" => Arg::Z,
        "w" => Arg::W,
        n => {
            Arg::Number(i64::from_str_radix(n, 10).expect("Could not parse as register or number"))
        }
    }
}

fn parse_instruction(s: &str) -> Instruction {
    let words = s.split(" ").collect::<Vec<&str>>();
    match words[..] {
        ["inp", a] => Instruction::Input { dest: parse_arg(a) },
        ["add", a, b] => Instruction::Add {
            dest: parse_arg(a),
            source: parse_arg(b),
        },
        ["mul", a, b] => Instruction::Mul {
            dest: parse_arg(a),
            source: parse_arg(b),
        },
        ["div", a, b] => Instruction::Div {
            dest: parse_arg(a),
            source: parse_arg(b),
        },
        ["mod", a, b] => Instruction::Mod {
            dest: parse_arg(a),
            source: parse_arg(b),
        },
        ["eql", a, b] => Instruction::Eql {
            dest: parse_arg(a),
            source: parse_arg(b),
        },
        ["ne", a, b] => Instruction::Ne {
            dest: parse_arg(a),
            source: parse_arg(b),
        },
        ["mov", a, b] => Instruction::Mov {
            dest: parse_arg(a),
            source: parse_arg(b),
        },
        _ => panic!("couldn't parse instruction"),
    }
}

fn read_problem_from_str(program: &str) -> Vec<Instruction> {
    program
        .split(";")
        .map(str::trim)
        .filter(|l| l.len() > 1)
        .filter(|l| !l.starts_with("#"))
        .map(parse_instruction)
        .collect()
}

fn read_problem(path: &str) -> Vec<Instruction> {
    fs::read_to_string(path)
        .expect("failed to read file")
        .trim()
        .to_string()
        .lines()
        .filter(|l| l.len() > 1)
        .filter(|l| !l.starts_with("#"))
        .map(parse_instruction)
        .collect()
}

fn read_partial_problem(path: &str) -> Vec<Instruction> {
    fs::read_to_string(path)
        .expect("failed to read file")
        .trim()
        .to_string()
        .lines()
        .filter(|l| l.len() > 1)
        .take_while(|l| !l.contains("="))
        .filter(|l| !l.starts_with("#"))
        .map(parse_instruction)
        .collect()
}

fn set(s: &mut State, reg: &Arg, value: i64) {
    match reg {
        Arg::X => s.x = value,
        Arg::Y => s.y = value,
        Arg::Z => s.z = value,
        Arg::W => s.w = value,
        _ => panic!("can't set a number"),
    }
}

fn get(s: &State, reg: &Arg) -> i64 {
    match reg {
        Arg::X => s.x,
        Arg::Y => s.y,
        Arg::Z => s.z,
        Arg::W => s.w,
        Arg::Number(n) => *n,
    }
}

#[derive(Debug)]
struct InterpreterError {
    msg: String,
    arg: Arg,
}

impl InterpreterError {
    fn new(msg: &str, arg: &Arg) -> InterpreterError {
        InterpreterError {
            msg: msg.to_string(),
            arg: *arg,
        }
    }
}

impl fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} with var {}", self.msg, self.arg)
    }
}

type InterpreterResult = Result<State, InterpreterError>;

fn apply<'a>(
    s: &mut State,
    i: &Instruction,
    mut input: impl Iterator<Item = &'a i64>,
) -> Result<(), InterpreterError> {
    match i {
        Instruction::Input { dest } => set(s, dest, *input.next().expect("ran out of input")),
        Instruction::Add { dest, source } => set(s, dest, get(s, dest) + get(s, source)),
        Instruction::Mul { dest, source } => set(s, dest, get(s, dest) * get(s, source)),
        Instruction::Div { dest, source } => {
            let rhs = get(s, source);
            if rhs == 0 {
                return Err(InterpreterError::new("division by zero", source));
            }
            set(s, dest, get(s, dest) / rhs);
        }
        Instruction::Mod { dest, source } => {
            let lhs = get(s, dest);
            if lhs < 0 {
                return Err(InterpreterError::new("mod with negative lhs", dest));
            }
            let rhs = get(s, source);
            if rhs <= 0 {
                return Err(InterpreterError::new("mod with rhs <= 0", source));
            }
            set(s, dest, lhs % rhs);
        }
        Instruction::Mov { dest, source } => set(s, dest, get(s, source)),
        Instruction::Eql { dest, source } => {
            set(s, dest, if get(s, dest) == get(s, source) { 1 } else { 0 })
        }
        Instruction::Ne { dest, source } => {
            set(s, dest, if get(s, dest) != get(s, source) { 1 } else { 0 })
        }
    };
    Ok(())
}

fn run<'a>(
    program: &Vec<Instruction>,
    mut input: impl Iterator<Item = &'a i64>,
) -> Result<State, InterpreterError> {
    let mut s = init_state();
    for p in program {
        apply(&mut s, p, &mut input)?;
    }
    Ok(s)
}

fn run_vec<'a>(program: &Vec<Instruction>, mut input: impl Iterator<Item = &'a i64>) -> Vec<State> {
    let mut s = init_state();
    let mut result = Vec::new();
    for p in program {
        apply(&mut s, p, &mut input);
        result.push(s);
    }
    result
}

fn rand_digit() -> i64 {
    let mut rng = rand::thread_rng();
    let dis = Uniform::new_inclusive(1, 9);
    rng.sample(dis)
}

fn rand_input() -> Vec<i64> {
    let mut result = Vec::new();
    for _ in 0..14 {
        result.push(rand_digit());
    }
    return result;
}

fn zeros() -> Vec<i64> {
    vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
}

fn ones() -> Vec<i64> {
    vec![1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
}

fn number_to_vec(i: i64) -> Vec<i64> {
    i.to_string()
        .chars()
        .map(|c| i64::from_str_radix(&c.to_string(), 10).unwrap())
        .collect()
}

struct ProgramConstant {
    a: i64,
    b: i64,
}

impl ProgramConstant {
    const fn new(a: i64, b: i64) -> ProgramConstant {
        ProgramConstant { a: a, b: b }
    }
}

const PROGRAM_CONSTANTS: [ProgramConstant; 14] = [
    ProgramConstant::new(10, 12),  // 0
    ProgramConstant::new(12, 7),   // 1
    ProgramConstant::new(10, 8),   // 2
    ProgramConstant::new(12, 8),   // 3
    ProgramConstant::new(11, 15),  // 4
    ProgramConstant::new(-16, 12), // 5
    ProgramConstant::new(10, 8),   // 6
    ProgramConstant::new(-11, 13), // 7
    ProgramConstant::new(-13, 3),  // 8
    ProgramConstant::new(13, 13),  // 9
    ProgramConstant::new(-8, 3),   // 10
    ProgramConstant::new(-1, 9),   // 11
    ProgramConstant::new(-4, 4),   // 12
    ProgramConstant::new(-14, 13), // 13
];

fn program<'a>(input: impl Iterator<Item = &'a i64>) -> InterpreterResult {
    let mut s: State = init_state();
    for (&w, constants) in izip!(input, PROGRAM_CONSTANTS) {
        s = program_block(&s, w, &constants)?;
    }
    Ok(s)
}

fn program_block(s: &State, w: i64, pc: &ProgramConstant) -> InterpreterResult {
    let mut state: State = *s;

    // peek
    if state.z < 0 {
        return Err(InterpreterError::new("mod with negative lhs", &Arg::Z));
    }
    state.x = state.z % 26;

    // pop
    if pc.a < 0 {
        state.z /= 26
    };

    // push w+b if and only if it's *not* the desired digit
    if w - pc.a != state.x {
        state.z *= 26;
        state.z += w + pc.b;
    }

    // necessary condition: input == (z % 26) plus -14
    Ok(state)

    // state.w = w;
    // state.x = state.z % 26;
    // if div_z {
    //     state.z /= 26
    // };
    // state.x += a;
    // state.x = if state.w != state.x { 1 } else { 0 };
    // state.y = 25 * state.x;
    // state.y += 1;
    // state.z *= state.y;
    // state.y = state.w;
    // state.y += b;
    // state.y *= state.x;
    // state.z += state.y;
}

fn will_succeed(w: &[i64]) -> bool {
    [
        w[13] == w[0] - 2,
        w[12] == w[1] + 3,
        w[11] == w[2] + 7,
        w[8] == w[3] - 5,
        w[5] == w[4] - 1,
        w[7] == w[6] - 3,
        w[10] == w[9] + 5,
    ]
    .iter()
    .all(|x| *x)
}

fn main() {
    // let prog = read_problem("../input.txt");
    // let mut input = vec![1, 1, 1, 1, 2, 1];
    // println!("{}", program(input.iter()).unwrap());

    // let mut states: Vec<(State, Vec<i64>)> = vec![(init_state(), vec![])];

    // for digit_idx in 0..=13 {
    //     let mut successor_states = Vec::new();
    //     for (state, input) in states.iter() {
    //         for do_push in [true, false] {
    //             let constants = &PROGRAM_CONSTANTS[digit_idx];
    //             let stack = int_to_stack(state.z);
    //             let stack_front = match stack.front() {
    //                 Some(n) => n,
    //                 None => &0,
    //             };
    //             let matching_digit = stack_front + constants.a;
    //             let next_digit = if do_push {
    //                 (0..=9).filter(|x| *x != matching_digit).max().unwrap()
    //             } else {
    //                 if matching_digit < 1 {
    //                     continue;
    //                 }
    //                 matching_digit
    //             };
    //             let mut next_input = input.to_vec();
    //             next_input.push(next_digit);
    //             let mut next_state = state.clone();
    //             program_block(&mut next_state, next_digit, constants);
    //             successor_states.push((next_state, next_input));
    //         }
    //     }
    //     states = successor_states;
    // }
    // let successful_states: Vec<&(State, Vec<i64>)> =
    //     states.iter().filter(|(s, _)| s.success()).collect();
    // dbg![successful_states];

    // 0
    // 0, 1
    // 0, 1, 2
    // 0, 1, 2, 3
    // 0, 1, 2, 3, 4
    // if w5+16 == w4+15: 0, 1, 2, 3
    // 0, 1, 2, 3, 6
    // if w7+11 == w6+8: 0, 1, 2, 3
    // if w8+13 == w3+8: 0, 1, 2
    // 0, 1, 2, 9
    // if w10+8 == w9+13: 0, 1, 2
    // if w11+1 == w2+8: 0, 1
    // if w12+4 == w1+7: 0
    // if w13+14 == w0+12:

    // w13 == w0-2
    // w12 == w1+3
    // w11 == w2+7
    // w8 == w3-5
    // w5 == w4-1
    // w7 == w6-3
    // w10 == w9+5

    let max_input = number_to_vec(96299896449997);
    dbg![will_succeed(&max_input)];
    dbg![program(max_input.iter())];

    let min_input = number_to_vec(31162141116841);
    dbg![will_succeed(&min_input)];
    dbg![program(min_input.iter())];

    // always push w0 + 12
    // always push w1 + 7
    // always push w2 + 8
    // always push w3 + 8
    // always push w4 + 15
    // pop
    // push w5+12 if w5+16 = pop
    // always push w6 + 8
    // pop
    // push w7+13 if w7+11 = pop
    // pop
    // push w8+3 if w8+13 = pop
    // always push w9 + 13
    // pop
    // push w10+3 if w10+8 = pop
    // pop
    // push w11+9 if w11+1 = pop
    // pop
    // push w12+4 if w12+4 = pop
    // pop
    // push w13+13 if w13+14 = pop

    // loop {
    //     input = rand_input();
    //     if let Ok(result) = run(&prog, input.iter()) {
    //         if result.success() {
    //             break;
    //         }
    //     }
    // }

    // dbg![program(input.iter())];

    // for idx in 0..14 {
    //     let mut min_digit = 1;
    //     let mut min_z = i64::MAX;
    //     for digit in 1..=9 {
    //         input[idx] = digit;
    //         match run(&prog, input.iter()) {
    //             Ok(result) => {
    //                 if result.success() {
    //                     if result.z < min_z {
    //                         min_digit = digit;
    //                     }
    //                 }
    //             }
    //             Err(e) => println!("{}", e),
    //         }
    //     }
    //     input[idx] = min_digit;
    // }

    // dbg![input];

    // let original_result = run(&prog, input.iter()).unwrap();
    // let reimplemented_result = program(input.iter());
    // dbg![input, original_result, reimplemented_result];

    // for i in (0..=99999999999999).rev() {
    //     let input = number_to_vec(i);
    //     let result = run(&program, input.iter());
    //     if i % 10000000 == 0 {
    //         dbg!(i);
    //     }
    //     if result.success() {
    //         dbg!(i, result);
    //         return;
    //     }
    // }

    // println!(
    //     "{}",
    //     program
    //         .iter()
    //         .map(Instruction::to_string)
    //         .collect::<Vec<String>>()
    //         .join("\n")
    // );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ex1_parse() {
        let expected_prog = vec![
            Instruction::Input { dest: Arg::X },
            Instruction::Mul {
                dest: Arg::X,
                source: Arg::Number(-1),
            },
        ];
        let prog = read_problem("../example1.txt");
        assert_eq!(prog, expected_prog);
    }

    #[test]
    fn ex1_runs() {
        let prog = read_problem("../example1.txt");
        let state = run(&prog, vec![3i64].iter()).unwrap();
        assert_eq!(state.x, -3);
    }

    #[test]
    fn ex2_parse() {
        let expected_prog = vec![
            Instruction::Input { dest: Arg::Z },
            Instruction::Input { dest: Arg::X },
            Instruction::Mul {
                dest: Arg::Z,
                source: Arg::Number(3),
            },
            Instruction::Eql {
                dest: Arg::Z,
                source: Arg::X,
            },
        ];
        let prog = read_problem("../example2.txt");
        assert_eq!(prog, expected_prog);
    }

    #[test]
    fn ex2_runs1() {
        let prog = read_problem("../example2.txt");
        let state = run(&prog, vec![3i64, 9i64].iter()).unwrap();
        assert_eq!(state.z, 1);
    }

    #[test]
    fn ex2_runs2() {
        let prog = read_problem("../example2.txt");
        let state = run(&prog, vec![3i64, 8i64].iter()).unwrap();
        assert!(state.success());
    }

    #[test]
    fn ex3_runs1() {
        let prog = read_problem("../example3.txt");
        let state = run(&prog, vec![1i64].iter()).unwrap();
        assert_eq!(state.z, 1);
        assert_eq!(state.y, 0);
        assert_eq!(state.x, 0);
        assert_eq!(state.w, 0);
    }

    #[test]
    fn ex3_runs2() {
        let prog = read_problem("../example3.txt");
        let state = run(&prog, vec![15i64].iter()).unwrap();
        assert_eq!(state.z, 1);
        assert_eq!(state.y, 1);
        assert_eq!(state.x, 1);
        assert_eq!(state.w, 1);
    }

    #[test]
    fn ex3_runs3() {
        let prog = read_problem("../example3.txt");
        let state = run(&prog, vec![14i64].iter()).unwrap();
        assert_eq!(state.z, 0);
        assert_eq!(state.y, 1);
        assert_eq!(state.x, 1);
        assert_eq!(state.w, 1);
    }

    #[test]
    fn min_is_same() {
        let original = read_problem("../input.txt");
        let minimized = read_problem("../input-min.txt");
        for _ in 0..20000 {
            let input = rand_input();
            if let Ok(original_result) = run(&original, input.iter()) {
                if let Ok(minimized_result) = run(&minimized, input.iter()) {
                    assert_eq!(original_result.success(), minimized_result.success());
                }
            }
        }
    }

    #[test]
    fn reimplementation_is_same() {
        let original = read_problem("../input.txt");
        for _ in 0..20000 {
            let input = rand_input();
            if let Ok(original_result) = run(&original, input.iter()) {
                if let Ok(reimplemented_result) = program(input.iter()) {
                    assert_eq!(original_result.z, reimplemented_result.z);
                }
            }
        }
    }

    #[test]
    fn single_block_is_same() {
        let original = read_problem("../input_part0.txt");
        for i in 1..=9 {
            let input = vec![i];
            if let Ok(original_result) = run(&original, input.iter()) {
                if let Ok(reimplemented_result) =
                    program_block(&init_state(), i, &PROGRAM_CONSTANTS[0])
                {
                    assert_eq!(original_result.z, reimplemented_result.z);
                }
            }
        }
    }

    // #[test]
    // fn large_number() {
    //     let prog = read_problem("../input.txt");
    //     let input = number_to_vec(9999);
    //     let result = run(&prog, input.iter());
    //     assert!(result.success());
    // }
}
