// The Swift Programming Language
// https://docs.swift.org/swift-book

import Foundation
import Algorithms

enum Day4Error : Error {
    case InvalidApplication
}

func splitStringToChars(_ s: String) -> [Character] {
    var result: [Character] = []
    for c in s {
        result.append(c)
    }
    return result
}

let LETTERS = splitStringToChars("abcdefg")

let DIGITS_TO_SEGMENTS: [Int : [Character]] = [
  0: splitStringToChars("abcefg"),
  1: splitStringToChars("cf"),
  2: splitStringToChars("acdeg"),
  3: splitStringToChars("acdfg"),
  4: splitStringToChars("bcdf"),
  5: splitStringToChars("abdfg"),
  6: splitStringToChars("abdefg"),
  7: splitStringToChars("acf"),
  8: splitStringToChars("abcdefg"),
  9: splitStringToChars("abcdfg"),
]

let SEGMENTS_TO_DIGITS: [[Character] : Int] = DIGITS_TO_SEGMENTS.reduce(into: [[Character]: Int]()) {
    (collect, a) in collect[a.value] = a.key
}

let UNIQUES = [
  DIGITS_TO_SEGMENTS[1]!,
  DIGITS_TO_SEGMENTS[4]!,
  DIGITS_TO_SEGMENTS[7]!,
  DIGITS_TO_SEGMENTS[8]!,
]

let UNIQUE_LENGTHS = [2, 3, 4, 7]

let DATA_PATHS = [
  "example1.txt",
  "example2.txt",
  "input.txt",
]

func readInput(_ path: String) -> String {
    let dir = try! FileManager.default.url(for: .documentDirectory, in: .userDomainMask, appropriateFor: nil, create: false)
    let fileURL = dir.appendingPathComponent(path)
    return try! String(contentsOf: fileURL, encoding: .utf8)
}

func parseLine<S: StringProtocol>(_ line: S) -> ([[Character]], [[Character]]) {
    var started = false
    var observed: [[Character]] = []
    var output: [[Character]] = []
    for word in line.split(separator: " ") {
        if word == "|" {
            started = true
        } else if !started {
            observed.append(word.sorted())
        } else {
            output.append(word.sorted())
        }
    }
    return (observed, output)
}

func listEasy(_ observed: [[Character]], _ output: [[Character]]) -> [[Character]] {
    var result: [[Character]] = []
    for word in output {
        if UNIQUE_LENGTHS.contains(word.count) {
            result.append(word)
        }
    }
    return result
}


func countEasy(_ observed: [[Character]], _ output: [[Character]]) -> Int {
    return listEasy(observed, output).count
}

func apply(_ perm: [Character:Character], _ words: [[Character]]) -> [[Character]] {
    var result: [[Character]] = []
    for w in words {
        result.append(w.map({perm[$0]!}).sorted())
    }
    return result
}

func solves(_ perm: [Character:Character], _ observed: [[Character]], _ output : [[Character]]) -> Bool {
    // if we can't find all of the "easy" configurations, reject
    let mappedObserved: [[Character]] = apply(perm, output)
    for obs in mappedObserved {
        for unique in UNIQUES {
            if unique.count == obs.count && unique != obs {
                return false
            }
        }
    }

    // if we produce any incomprehensible digits, reject
    let mappedOutput: [[Character]] = apply(perm, observed)
    for out in mappedOutput {
        if let _ = SEGMENTS_TO_DIGITS[out] {
        } else {
            return false
        }
    }

    return true
}

func solveEntry(_ observed: [[Character]], _ output: [[Character]]) throws -> Int {
    for order in "abcdefg".permutations(ofCount: 7) {
        let perm = Dictionary(uniqueKeysWithValues: zip(LETTERS, order))
        if solves(perm, observed, output) {
            return apply(perm, output)
              .map( { SEGMENTS_TO_DIGITS[$0]! })
              .reduce(0, { acc, el in acc * 10 + el })
        }
    }
    throw Day4Error.InvalidApplication
}


@main
struct Day8 {
    static func main() {
        for input in DATA_PATHS {
            print(input)
            let inputfile = readInput(input)
            let lines = inputfile.split(separator: "\n").map(parseLine)

            var result1 = 0
            for (observed, output) in lines {
                let easy = countEasy(observed, output)
                result1 += easy
            }
            print("part 1", result1)

            var result2 = 0
            for (observed, input) in lines {
                result2 += try! solveEntry(observed, input);
            }
            print("part 2", result2)
        }
    }
}
