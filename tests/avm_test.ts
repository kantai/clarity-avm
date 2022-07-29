// deno-lint-ignore-file
import { Clarinet, Tx, Chain, Account, Contract, types } from 'https://deno.land/x/clarinet@v0.33.0/index.ts';
import { assertEquals } from "https://deno.land/std@0.90.0/testing/asserts.ts";
import { createHash } from "https://deno.land/std@0.107.0/hash/mod.ts";
import { decode as decHex, encode as encHex } from "https://deno.land/std@0.149.0/encoding/hex.ts";

function fromHex(input: string) {
    const hexBytes = new TextEncoder().encode(input);
    return decHex(hexBytes);
}

function toHex(input: Uint8Array) {
    const hexBytes = encHex(input);
    return new TextDecoder().decode(hexBytes);
}

const AVM = "avm";

const OP_NOP = types.buff(fromHex("3b"));
const OP_LOG = types.buff(fromHex("61"));
const OP_LT = types.buff(fromHex("10"));
const OP_DUP1 = types.buff(fromHex("41"));
const OP_ADD = types.buff(fromHex("01"));
const OP_SWP2 = types.buff(fromHex("44"));
const OP_MUL = types.buff(fromHex("02"));
const OP_CJUMP = types.buff(fromHex("35"));

const CLAR_256 = types.buff(fromHex("0100000000000000000000000000000100"));
const CLAR_2 = types.buff(fromHex("0100000000000000000000000000000002"));
const CLAR_1 = types.buff(fromHex("0100000000000000000000000000000001"));
const CLAR_0 = types.buff(fromHex("0100000000000000000000000000000000"));
const CLAR_PC_2 = types.buff(fromHex("0c00000001037063720100000000000000000000000000000009"));

Clarinet.test({
    name: "Unit tests for invert byte",
    async fn(chain: Chain, accounts: Map<string, Account>, contracts: Map<string, Contract>) {
        const alice = accounts.get("wallet_1")!;
        const tests = [
            { out: "3d", in: "c2"},
            { out: "fa", in: "05"},
            { out: "c3", in: "3c"},
            { out: "6e", in: "91"},
            { out: "49", in: "b6"},
            { out: "eb", in: "14"},
            { out: "56", in: "a9"},
            { out: "c5", in: "3a"},
            { out: "c2", in: "3d"},
            { out: "3f", in: "c0"},
            { out: "6c", in: "93"},
            { out: "b2", in: "4d"},
            { out: "48", in: "b7"},
            { out: "f8", in: "07"},
            { out: "3c", in: "c3"},
            { out: "fb", in: "04"},
            { out: "ba", in: "45"},
            { out: "75", in: "8a"},
            { out: "ec", in: "13"},
            { out: "af", in: "50"},
            { out: "3d", in: "c2"},
            { out: "19", in: "e6"},
            { out: "9b", in: "64"},
            { out: "b4", in: "4b"},
            { out: "94", in: "6b"},
            { out: "4e", in: "b1"},
            { out: "f8", in: "07"},
            { out: "84", in: "7b"},
            { out: "96", in: "69"},
            { out: "d0", in: "2f"},
            { out: "71", in: "8e"},
            { out: "59", in: "a6"},
            { out: "ff", in: "00"},
        ];
        for (const test of tests) {
            assertEquals(chain.callReadOnlyFn(AVM, "invert-byte", [types.buff(fromHex(test.in))], alice.address).result, `0x${test.out}`);
            assertEquals(chain.callReadOnlyFn(AVM, "invert-byte", [types.buff(fromHex(test.out))], alice.address).result, `0x${test.in}`);
        }
    }
})

Clarinet.test({
    name: "Test a one line program",
    async fn(chain: Chain, accounts: Map<string, Account>, contracts: Map<string, Contract>) {
        const alice = accounts.get("wallet_1")!;
        const instructions = types.list([
            types.tuple({ "op": OP_LOG, "imv": types.some(types.buff(fromHex("00"))) })
        ]);


        let execution = chain.mineBlock([
            Tx.contractCall(AVM, "initialize-vm", [instructions], alice.address),
            Tx.contractCall(AVM, "run-ten", [], alice.address),
            Tx.contractCall(AVM, "run-one", [], alice.address),
        ]);

        let run_out = execution.receipts[1];
        let run_again = execution.receipts[2];
        assertEquals(run_out.result, run_again.result, "Both executions should halt when reaching final instruction");
        let log_events = run_out.events.filter((x) => { return x.contract_event.topic == "print" && x.contract_event.value.search("avm-log") != -1 });
        assertEquals(log_events.length, 1);
        assertEquals(log_events[0].contract_event.value, "{avm-log: 0x00}");
    }
});


Clarinet.test({
    name: "Test a longer program: log2",
    async fn(chain: Chain, accounts: Map<string, Account>, contracts: Map<string, Contract>) {
        const alice = accounts.get("wallet_1")!;
        const instructions = [
            // log2(a, b, c) = {
            //    if b > a:
            //       return c
            //    else:
            //       return log2(a, b*2, c+1)
            types.tuple({ "op": OP_NOP, "imv": types.some(CLAR_1) }),
            types.tuple({ "op": OP_NOP, "imv": types.some(CLAR_256) }),
            types.tuple({ "op": OP_NOP, "imv": types.some(CLAR_2) }),
            types.tuple({ "op": OP_SWP2, "imv": types.none() }),
            types.tuple({ "op": OP_ADD, "imv": types.some(CLAR_1) }),
            types.tuple({ "op": OP_SWP2, "imv": types.none() }),
            types.tuple({ "op": OP_MUL, "imv": types.some(CLAR_2) }),
            types.tuple({ "op": OP_DUP1, "imv": types.none() }),
            types.tuple({ "op": OP_DUP1, "imv": types.none() }),
            types.tuple({ "op": OP_LT, "imv": types.none() }),
            types.tuple({ "op": OP_CJUMP, "imv": types.some(CLAR_PC_2) }),
            types.tuple({ "op": OP_SWP2, "imv": types.none() }),
            types.tuple({ "op": OP_LOG, "imv": types.none() }),
        ];

        instructions.reverse();

        let execution = chain.mineBlock([
            Tx.contractCall(AVM, "initialize-vm", [types.list(instructions)], alice.address),
            Tx.contractCall(AVM, "run-100", [], alice.address),
        ]);

        const out = execution.receipts[1];
        assertEquals(out.result, "(ok false)", "Execution should halt when running log(256)");

        // these are calls to `log` instruction
        let log_events = out.events.filter((x) => { return x.contract_event.topic == "print" && x.contract_event.value.search("avm-log") != -1 });
        assertEquals(log_events.length, 1);
        assertEquals(log_events[0].contract_event.value, "{avm-log: 0x0100000000000000000000000000000008}");

        // these are print statements logging each step of execution
        let step_events = out.events.filter((x) => { return x.contract_event.topic == "print" && x.contract_event.value.search("data-stack-tail") != -1 }).map(x => x.contract_event.value);
        // console.log(step_events);
    }
});
