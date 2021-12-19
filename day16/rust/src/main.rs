use num_bigint::BigUint;
use num_bigint::ToBigUint;
use num_traits::FromPrimitive;
use num_traits::ToPrimitive;
use num_traits::{One, Zero};

#[macro_use]
use num_derive::FromPrimitive;

use std::char;
use std::fs;

#[derive(Debug, PartialEq, FromPrimitive)]
enum Op {
    Sum = 0,
    Product = 1,
    Minimum = 2,
    Maximum = 3,
    Gt = 5,
    Lt = 6,
    Eq = 7,
}

#[derive(Debug, PartialEq)]
enum Packet {
    Operator {
        op: Op,
        version: u8,
        contents: Vec<Packet>,
    },
    Literal {
        version: u8,
        value: BigUint,
    },
}

type Bits<'a> = &'a [bool];

fn bits_to_number(bits: Bits) -> BigUint {
    return bits
        .iter()
        .fold(Zero::zero(), |acc, bit| 2u8 * acc + (*bit as u8));
}

fn bits_to_bitstring(bits: Bits) -> String {
    return bits.iter().map(|el| if *el { '1' } else { '0' }).collect();
}

fn bits_to_hex(bits: Bits) -> String {
    bits.chunks(4)
        .map(bits_to_number)
        .map(|n| char::from_digit(n.to_u32().unwrap(), 16))
        .map(Option::unwrap)
        .map(|s| s.to_ascii_uppercase())
        .collect()
}

fn hex_to_bits(s: &str) -> Vec<bool> {
    return s
        .chars()
        .map(|c| u8::from_str_radix(&c.to_string(), 16))
        .map(Result::unwrap)
        .flat_map(|n| {
            return [(n / 8) % 2, (n / 4) % 2, (n / 2) % 2, n % 2];
        })
        .map(|el| el != 0)
        .collect();
}

fn parse_varint(bits: Bits) -> (BigUint, Bits) {
    fn helper(bits: Bits, acc: BigUint) -> (BigUint, Bits) {
        let next_acc = bits[1..5].iter().fold(acc, |a, b| (a * 2u8) + *b as u8);
        let has_next_chunk = bits[0];
        return if has_next_chunk {
            helper(&bits[5..], next_acc)
        } else {
            (next_acc, &bits[5..])
        };
    }
    return helper(bits, Zero::zero());
}

fn parse_literal(version: u8, data_bits: Bits) -> (Packet, Bits) {
    let (value, rest) = parse_varint(data_bits);
    let p = Packet::Literal { version, value };
    return (p, rest);
}

fn parse_operator(op: Op, version: u8, data_bits: Bits) -> (Packet, Bits) {
    let mut contents = Vec::new();
    let mut rest = &data_bits[1..];
    if data_bits[0] {
        // next 11 bits are packet length in sub-packets
        let num_packets = bits_to_number(&rest[..11]).to_u32().unwrap();
        rest = &rest[11..];
        for _ in 0..num_packets {
            let out = parse(rest);
            rest = out.1;
            contents.push(out.0);
        }
    } else {
        // next 15 bits are packet length in bits
        let num_bits = bits_to_number(&rest[..15]).to_usize().unwrap();
        rest = &rest[15..];
        let mut unparsed = &rest[0..num_bits];
        rest = &rest[num_bits..];
        while unparsed.len() > 0 {
            let out = parse(unparsed);
            unparsed = out.1;
            contents.push(out.0);
        }
    }
    let p = Packet::Operator {
        op,
        version,
        contents,
    };
    return (p, rest);
}

// Returns whatever gets parsed plus any un-parsed bits
fn parse(bits: Bits) -> (Packet, Bits) {
    let version = bits_to_number(&bits[0..3]).to_u8().unwrap();
    let id = bits_to_number(&bits[3..6]).to_u8().unwrap();
    let data_bits = &bits[6..];
    return match id {
        4 => parse_literal(version, data_bits),
        _ => parse_operator(
            Op::from_u8(id).expect("unknown packet type id"),
            version,
            data_bits,
        ),
    };
}

fn version_sum(p: &Packet) -> u32 {
    match p {
        Packet::Operator {
            version, contents, ..
        } => {
            let contents_version: u32 = contents.iter().map(version_sum).sum();
            (*version as u32) + contents_version
        }
        Packet::Literal { version, .. } => (*version).into(),
    }
}

fn value(p: &Packet) -> BigUint {
    match p {
        Packet::Literal { value, .. } => value.clone(),
        Packet::Operator {
            op: Op::Sum,
            contents,
            ..
        } => contents.iter().map(value).sum(),
        Packet::Operator {
            op: Op::Product,
            contents,
            ..
        } => contents.iter().map(value).product(),
        Packet::Operator {
            op: Op::Minimum,
            contents,
            ..
        } => contents
            .iter()
            .map(value)
            .min()
            .expect("no values in Min packet"),
        Packet::Operator {
            op: Op::Maximum,
            contents,
            ..
        } => contents
            .iter()
            .map(value)
            .max()
            .expect("no values in Max packet"),
        Packet::Operator {
            op: Op::Gt,
            contents,
            ..
        } => {
            if value(&contents[0]) > value(&contents[1]) {
                One::one()
            } else {
                Zero::zero()
            }
        }
        Packet::Operator {
            op: Op::Lt,
            contents,
            ..
        } => {
            if value(&contents[0]) < value(&contents[1]) {
                One::one()
            } else {
                Zero::zero()
            }
        }
        Packet::Operator {
            op: Op::Eq,
            contents,
            ..
        } => {
            if value(&contents[0]) == value(&contents[1]) {
                One::one()
            } else {
                Zero::zero()
            }
        }
    }
}

fn main() {
    let input_str = fs::read_to_string("../input.txt")
        .expect("Could not read input.txt")
        .trim()
        .to_string();
    let bits = hex_to_bits(&input_str);
    let (result, _) = parse(&bits);
    let input_sum = version_sum(&result);
    dbg!(input_sum);
    let input_value = value(&result);
    dbg!(input_value);
}

#[cfg(test)]
mod tests {
    use super::*;

    fn bitstring_to_bits(bitstring: &str) -> Vec<bool> {
        return bitstring.chars().map(|c| c != '0').collect();
    }

    #[test]
    fn test_bitstring_to_bits() {
        assert_eq!(
            bitstring_to_bits("110100101111111000101000"),
            [
                true, true, false, true, false, false, true, false, true, true, true, true, true,
                true, true, false, false, false, true, false, true, false, false, false
            ]
        );
    }

    #[test]
    fn test_hex_to_bits() {
        assert_eq!(
            hex_to_bits("D2FE28"),
            bitstring_to_bits("110100101111111000101000")
        );
    }

    #[test]
    fn parse_to_literal() {
        let bits = hex_to_bits(&"D2FE28");
        let (result, rest) = parse(&bits);
        assert_eq!(
            result,
            Packet::Literal {
                version: 6,
                value: BigUint::from(2021u32),
            }
        );
        assert_eq!(rest, bitstring_to_bits("000"));
    }

    #[test]
    fn parse_operator_with_bits_length() {
        let bits = hex_to_bits(&"38006F45291200");
        let (result, rest) = parse(&bits);
        assert_eq!(
            result,
            Packet::Operator {
                op: Op::Lt,
                version: 1,
                contents: vec![
                    Packet::Literal {
                        version: 6,
                        value: 10u16.to_biguint().unwrap(),
                    },
                    Packet::Literal {
                        version: 2,
                        value: 20u16.to_biguint().unwrap(),
                    },
                ],
            }
        );
        assert_eq!(rest, bitstring_to_bits("0000000"));
    }

    #[test]
    fn parse_operator_with_packets_length() {
        let bits = hex_to_bits(&"EE00D40C823060");
        let (result, rest) = parse(&bits);
        assert_eq!(
            result,
            Packet::Operator {
                op: Op::Maximum,
                version: 7,
                contents: vec![
                    Packet::Literal {
                        version: 2,
                        value: 1u16.to_biguint().unwrap(),
                    },
                    Packet::Literal {
                        version: 4,
                        value: 2u16.to_biguint().unwrap(),
                    },
                    Packet::Literal {
                        version: 1,
                        value: 3u16.to_biguint().unwrap(),
                    },
                ],
            }
        );
        assert_eq!(rest, bitstring_to_bits("00000"));
    }

    #[test]
    fn version_sum_1() {
        let bits = hex_to_bits(&"8A004A801A8002F478");
        let (result, _) = parse(&bits);
        let vs = version_sum(&result);
        assert_eq!(vs, 16);
    }

    #[test]
    fn version_sum_2() {
        let bits = hex_to_bits(&"620080001611562C8802118E34");
        let (result, _) = parse(&bits);
        let vs = version_sum(&result);
        assert_eq!(vs, 12);
    }

    #[test]
    fn version_sum_3() {
        let bits = hex_to_bits(&"C0015000016115A2E0802F182340");
        let (result, _) = parse(&bits);
        let vs = version_sum(&result);
        assert_eq!(vs, 23);
    }

    #[test]
    fn version_sum_4() {
        let bits = hex_to_bits(&"A0016C880162017C3686B18A3D4780");
        let (result, _) = parse(&bits);
        let vs = version_sum(&result);
        assert_eq!(vs, 31);
    }

    #[test]
    fn value_1() {
        let bits = hex_to_bits(&"C200B40A82");
        let (result, _) = parse(&bits);
        let value = value(&result);
        assert_eq!(value, BigUint::from_u32(3).unwrap());
    }

    #[test]
    fn value_2() {
        let bits = hex_to_bits(&"04005AC33890");
        let (result, _) = parse(&bits);
        let value = value(&result);
        assert_eq!(value, BigUint::from_u32(54).unwrap());
    }

    #[test]
    fn value_3() {
        let bits = hex_to_bits(&"880086C3E88112");
        let (result, _) = parse(&bits);
        let value = value(&result);
        assert_eq!(value, BigUint::from_u32(7).unwrap());
    }

    #[test]
    fn value_4() {
        let bits = hex_to_bits(&"CE00C43D881120");
        let (result, _) = parse(&bits);
        let value = value(&result);
        assert_eq!(value, BigUint::from_u32(9).unwrap());
    }

    #[test]
    fn value_5() {
        let bits = hex_to_bits(&"D8005AC2A8F0");
        let (result, _) = parse(&bits);
        let value = value(&result);
        assert_eq!(value, BigUint::from_u32(1).unwrap());
    }

    #[test]
    fn value_6() {
        let bits = hex_to_bits(&"F600BC2D8F");
        let (result, _) = parse(&bits);
        let value = value(&result);
        assert_eq!(value, BigUint::from_u32(0).unwrap());
    }

    #[test]
    fn value_7() {
        let bits = hex_to_bits(&"9C005AC2F8F0");
        let (result, _) = parse(&bits);
        let value = value(&result);
        assert_eq!(value, BigUint::from_u32(0).unwrap());
    }

    #[test]
    fn value_8() {
        let bits = hex_to_bits(&"9C0141080250320F1802104A08");
        let (result, _) = parse(&bits);
        let value = value(&result);
        assert_eq!(value, BigUint::from_u32(1).unwrap());
    }
}
