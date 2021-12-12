import * as fs from 'fs';
import * as assert from 'assert';

interface Edge<T> {
    a: T
    b: T
}

interface Graph<T> {
    edges: Edge<T>[]
}

interface Path<T> {
    nodes: T[]
}

function hasUsedDoubleVisit<T>(path: Path<T>): boolean {
    const smalls = path.nodes.filter(a => !isBigRoom(a));
    const deduped = new Set(smalls);
    return deduped.size != smalls.length;
}

function parseGraph<T>(s: string, conv: (x: any) => T = x => x): Graph<T> {
    var g: Graph<T> = {edges: []};
    for (const line of s) {
        const edge: Edge<T> = {
            a: conv(line[0]),
            b: conv(line[1]),
        };
        g.edges.push(edge);
    }
    return g;
}

function isBigRoom(n: any): boolean {
    if (typeof n === "string") {
        return n === n.toUpperCase();
    }
    throw "isBigRoom unimplemented for type: " + typeof(n);
}

function canEnter1<T>(n: T, p: Path<T>): boolean {
    return isBigRoom(n) || !(p.nodes.includes(n));
}

function canEnter2<T>(start: T, end: T): (n: T, p: Path<T>) => boolean {
    return (n, p) => {
        if (isBigRoom(n)) {
            // Can always enter big rooms multiple times
            return true;
        }
        if (!p.nodes.includes(n)) {
            // Can always enter a small room for the first time
            return true;
        }
        // At this point we know that we're on our second visit to a small room.
        if (n === start || n === end) {
            // can't visit the start or end rooms twice
            return false;
        }
        if (hasUsedDoubleVisit(p)) {
            // Can't visit twice if we've already used our double-visit.
            return false;
        }
        // Otherwise we can use our double visit.
        return true;
    }
}

function* neighbors<T>(g: Graph<T>, node: T): Generator<T> {
    for (const edge of g.edges) {
        if (node == edge.a) {
            yield edge.b;
        } else if (node == edge.b) {
            yield edge.a;
        }
    }
}

function* traverse<T>(g: Graph<T>, end: T, path: Path<T>, can_enter: (n: T, p: Path<T>) => boolean): Generator<Path<T>> {
    if (path.nodes.includes(end)) {
        // clone the path, since we're mutating it as we go
        yield { nodes: Array.from(path.nodes) };
        return;
    }

    const last = path.nodes[path.nodes.length - 1];
    for (const n of neighbors(g, last)) {
        if (can_enter(n, path)) {
            path.nodes.push(n);
            yield* traverse(g, end, path, can_enter);
            path.nodes.pop();
        }
    }
}

function readProblem(fname: string): Graph<string> {
    const read = fs.readFileSync(fname);
    const data = JSON.parse(read.toString());
    return parseGraph<string>(data.problem);
}

// read everything

const exg0 = readProblem('../example.json');
const exg1 = readProblem('../example1.json');
const exg2 = readProblem('../example2.json');
const inp = readProblem('../input.json');

// Part 1

const ex0_paths1 = Array.from(traverse(exg0, 'end', {nodes: ['start']}, canEnter1));
assert.equal(ex0_paths1.length, 10);

const ex1_paths1 = Array.from(traverse(exg1, 'end', {nodes: ['start']}, canEnter1));
assert.equal(ex1_paths1.length, 19);

const ex2_paths1 = Array.from(traverse(exg2, 'end', {nodes: ['start']}, canEnter1));
assert.equal(ex2_paths1.length, 226);

const inp_paths1 = Array.from(traverse(inp, 'end', {nodes: ['start']}, canEnter1));
assert.equal(inp_paths1.length, 3761);

// Part 2


const ex0_paths2 = Array.from(traverse(exg0, 'end', {nodes: ['start']}, canEnter2("start", "end")));
assert.equal(ex0_paths2.length, 36);

const ex1_paths2 = Array.from(traverse(exg1, 'end', {nodes: ['start']}, canEnter2("start", "end")));
assert.equal(ex1_paths2.length, 103);

const ex2_paths2 = Array.from(traverse(exg2, 'end', {nodes: ['start']}, canEnter2("start", "end")));
assert.equal(ex2_paths2.length, 3509);

const inp_paths2 = Array.from(traverse(inp, 'end', {nodes: ['start']}, canEnter2("start", "end")));
console.log(inp_paths2.length);
