#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

int disassemble(unsigned char *codebuffer, int pc) {
    unsigned char *code = &codebuffer[pc];
    int opbytes = 1;
    printf("  %04x " , pc);
    switch(*code) {
        case 0x00: printf(" NOP"); break;
        case 0x01: printf(" LXI B, #$%02x%02x" , code[2], code[1]); opbytes=3; break;
        case 0x02: printf(" STAX B"); break;
        case 0x03: printf(" INX B"); break;
        case 0x04: printf(" INR B"); break;
        case 0x05: printf(" DCR B"); break;
        case 0x06: printf(" MVI B, #$%02x" , code[1]); opbytes=2;  break;
        case 0x07: printf(" RLC"); break;
        case 0x08: printf(" NOP"); break;
        case 0x09: printf(" DAD B"); break;
        case 0x0a: printf(" LDAX B"); break;
        case 0x0b: printf(" DCX B"); break;
        case 0x0c: printf(" INR C"); break;
        case 0x0d: printf(" DCR C"); break;
        case 0x0e: printf(" MVI C, #$%02x" , code[1]); opbytes=2; break;
        case 0x0f: printf(" RRC"); break;

        case 0x10: printf(" NOP"); break;
        case 0x11: printf(" LXI D, #$%02x%02x" , code[2], code[1]); opbytes=3; break;
        case 0x12: printf(" STAX D"); break;
        case 0x13: printf(" INX D"); break;
        case 0x14: printf(" INR D"); break;
        case 0x15: printf(" DCR D"); break;
        case 0x16: printf(" MVI D, #$%02x" , code[1]); opbytes=2;  break;
        case 0x17: printf(" RAL"); break;
        case 0x18: printf(" NOP"); break;
        case 0x19: printf(" DAD D"); break;
        case 0x1a: printf(" LDAX D"); break;
        case 0x1b: printf(" DCX D"); break;
        case 0x1c: printf(" INR E"); break;
        case 0x1d: printf(" DCR E"); break;
        case 0x1e: printf(" MVI E, #$%02x" , code[1]); opbytes=2; break;
        case 0x1f: printf(" RAR"); break;

        case 0x20: printf(" RIM"); break;
        case 0x21: printf(" LXI H, #$%02x%02x" , code[2], code[1]); opbytes=3; break;
        case 0x22: printf(" SHLD, #$%02x%02x" , code[2], code[1]); opbytes=3; break;
        case 0x23: printf(" INX H"); break;
        case 0x24: printf(" INR H"); break;
        case 0x25: printf(" DCR H"); break;
        case 0x26: printf(" MVI H, #$%02x" , code[1]); opbytes=2;  break;
        case 0x27: printf(" DAA"); break;
        case 0x28: printf(" NOP"); break;
        case 0x29: printf(" DAD H"); break;
        case 0x2a: printf(" LHLD, #$%02x%02x" , code[2], code[1]); opbytes=3; break;
        case 0x2b: printf(" DCX H"); break;
        case 0x2c: printf(" INR L"); break;
        case 0x2d: printf(" DCR L"); break;
        case 0x2e: printf(" MVI L, #$%02x" , code[1]); opbytes=2; break;
        case 0x2f: printf(" CMA"); break;

        case 0x30: printf(" SIM"); break;
        case 0x31: printf(" LXI SP, #$%02x%02x" , code[2], code[1]); opbytes=3; break;
        case 0x32: printf(" STA, #$%02x%02x" , code[2], code[1]); opbytes=3; break;
        case 0x33: printf(" INX SP"); break;
        case 0x34: printf(" INR M"); break;
        case 0x35: printf(" DCR M"); break;
        case 0x36: printf(" MVI M, #$%02x" , code[1]); opbytes=2;  break;
        case 0x37: printf(" STC"); break;
        case 0x38: printf(" NOP"); break;
        case 0x39: printf(" DAD SP"); break;
        case 0x3a: printf(" LDA, #$%02x%02x" , code[2], code[1]); opbytes=3; break;
        case 0x3b: printf(" DCX SP"); break;
        case 0x3c: printf(" INR A"); break;
        case 0x3d: printf(" DCR A"); break;
        case 0x3e: printf(" MVI A, #$%02x" , code[1]); opbytes=2; break;
        case 0x3f: printf(" CMC"); break;

        case 0x40: printf(" MOV B,B"); break;
        case 0x41: printf(" MOV B,C"); break;
        case 0x42: printf(" MOV B,D"); break;
        case 0x43: printf(" MOV B,E"); break;
        case 0x44: printf(" MOV B,H"); break;
        case 0x45: printf(" MOV B,L"); break;
        case 0x46: printf(" MOV B,M"); break;
        case 0x47: printf(" MOV B,A"); break;
        case 0x48: printf(" MOV C,B"); break;
        case 0x49: printf(" MOV C,C"); break;
        case 0x4a: printf(" MOV C,D"); break;
        case 0x4b: printf(" MOV C,E"); break;
        case 0x4c: printf(" MOV C,H"); break;
        case 0x4d: printf(" MOV C,L"); break;
        case 0x4e: printf(" MOV C,M"); break;
        case 0x4f: printf(" MOV C,A"); break;

        case 0x50: printf(" MOV D,B"); break;
        case 0x51: printf(" MOV D,C"); break;
        case 0x52: printf(" MOV D,D"); break;
        case 0x53: printf(" MOV D,E"); break;
        case 0x54: printf(" MOV D,H"); break;
        case 0x55: printf(" MOV D,L"); break;
        case 0x56: printf(" MOV D,M"); break;
        case 0x57: printf(" MOV D,A"); break;
        case 0x58: printf(" MOV E,B"); break;
        case 0x59: printf(" MOV E,C"); break;
        case 0x5a: printf(" MOV E,D"); break;
        case 0x5b: printf(" MOV E,E"); break;
        case 0x5c: printf(" MOV E,H"); break;
        case 0x5d: printf(" MOV E,L"); break;
        case 0x5e: printf(" MOV E,M"); break;
        case 0x5f: printf(" MOV E,A"); break;

        case 0x60: printf(" MOV H,B"); break;
        case 0x61: printf(" MOV H,C"); break;
        case 0x62: printf(" MOV H,D"); break;
        case 0x63: printf(" MOV H,E"); break;
        case 0x64: printf(" MOV H,H"); break;
        case 0x65: printf(" MOV H,L"); break;
        case 0x66: printf(" MOV H,M"); break;
        case 0x67: printf(" MOV H,A"); break;
        case 0x68: printf(" MOV L,B"); break;
        case 0x69: printf(" MOV L,C"); break;
        case 0x6a: printf(" MOV L,D"); break;
        case 0x6b: printf(" MOV L,E"); break;
        case 0x6c: printf(" MOV L,H"); break;
        case 0x6d: printf(" MOV L,L"); break;
        case 0x6e: printf(" MOV L,M"); break;
        case 0x6f: printf(" MOV L,A"); break;

        case 0x70: printf(" MOV M,B"); break;
        case 0x71: printf(" MOV M,C"); break;
        case 0x72: printf(" MOV M,D"); break;
        case 0x73: printf(" MOV M,E"); break;
        case 0x74: printf(" MOV M,H"); break;
        case 0x75: printf(" MOV M,L"); break;
        case 0x76: printf(" HLT"); break;
        case 0x77: printf(" MOV M,A"); break;
        case 0x78: printf(" MOV A,B"); break;
        case 0x79: printf(" MOV A,C"); break;
        case 0x7a: printf(" MOV A,D"); break;
        case 0x7b: printf(" MOV A,E"); break;
        case 0x7c: printf(" MOV A,H"); break;
        case 0x7d: printf(" MOV A,L"); break;
        case 0x7e: printf(" MOV A,M"); break;
        case 0x7f: printf(" MOV A,A"); break;

        case 0x80: printf(" ADD B"); break;
        case 0x81: printf(" ADD C"); break;
        case 0x82: printf(" ADD D"); break;
        case 0x83: printf(" ADD E"); break;
        case 0x84: printf(" ADD H"); break;
        case 0x85: printf(" ADD L"); break;
        case 0x86: printf(" ADD M"); break;
        case 0x87: printf(" ADD A"); break;
        case 0x88: printf(" ADC B"); break;
        case 0x89: printf(" ADC C"); break;
        case 0x8a: printf(" ADC D"); break;
        case 0x8b: printf(" ADC E"); break;
        case 0x8c: printf(" ADC H"); break;
        case 0x8d: printf(" ADC L"); break;
        case 0x8e: printf(" ADC M"); break;
        case 0x8f: printf(" ADC A"); break;

        case 0x90: printf(" SUB B"); break;
        case 0x91: printf(" SUB C"); break;
        case 0x92: printf(" SUB D"); break;
        case 0x93: printf(" SUB E"); break;
        case 0x94: printf(" SUB H"); break;
        case 0x95: printf(" SUB L"); break;
        case 0x96: printf(" SUB M"); break;
        case 0x97: printf(" SUB A"); break;
        case 0x98: printf(" SBB B"); break;
        case 0x99: printf(" SBB C"); break;
        case 0x9a: printf(" SBB D"); break;
        case 0x9b: printf(" SBB E"); break;
        case 0x9c: printf(" SBB H"); break;
        case 0x9d: printf(" SBB L"); break;
        case 0x9e: printf(" SBB M"); break;
        case 0x9f: printf(" SBB A"); break;

        case 0xa0: printf(" ANA B"); break;
        case 0xa1: printf(" ANA C"); break;
        case 0xa2: printf(" ANA D"); break;
        case 0xa3: printf(" ANA E"); break;
        case 0xa4: printf(" ANA H"); break;
        case 0xa5: printf(" ANA L"); break;
        case 0xa6: printf(" ANA M"); break;
        case 0xa7: printf(" ANA A"); break;
        case 0xa8: printf(" ANA B"); break;
        case 0xa9: printf(" ANA C"); break;
        case 0xaa: printf(" ANA D"); break;
        case 0xab: printf(" ANA E"); break;
        case 0xac: printf(" ANA H"); break;
        case 0xad: printf(" ANA L"); break;
        case 0xae: printf(" ANA M"); break;
        case 0xaf: printf(" ANA A"); break;

        case 0xb0: printf(" ORA B"); break;
        case 0xb1: printf(" ORA C"); break;
        case 0xb2: printf(" ORA D"); break;
        case 0xb3: printf(" ORA E"); break;
        case 0xb4: printf(" ORA H"); break;
        case 0xb5: printf(" ORA L"); break;
        case 0xb6: printf(" ORA M"); break;
        case 0xb7: printf(" ORA A"); break;
        case 0xb8: printf(" CMP B"); break;
        case 0xb9: printf(" CMP C"); break;
        case 0xba: printf(" CMP D"); break;
        case 0xbb: printf(" CMP E"); break;
        case 0xbc: printf(" CMP H"); break;
        case 0xbd: printf(" CMP L"); break;
        case 0xbe: printf(" CMP M"); break;
        case 0xbf: printf(" CMP A"); break;

        case 0xc0: printf(" RNZ"); break;
        case 0xc1: printf(" POP B"); break;
        case 0xc2: printf(" JNZ, #$%02x%02x" , code[2], code[1]); opbytes=3; break;
        case 0xc3: printf(" JMP, #$%02x%02x" , code[2], code[1]); opbytes=3; break;
        case 0xc4: printf(" CNZ, #$%02x%02x" , code[2], code[1]); opbytes=3; break;
        case 0xc5: printf(" PUSH B"); break;
        case 0xc6: printf(" ADI, #$%02x" , code[1]); opbytes=2;  break;
        case 0xc7: printf(" RST 0"); break;
        case 0xc8: printf(" RZ"); break;
        case 0xc9: printf(" RET"); break;
        case 0xca: printf(" JZ, #$%02x%02x" , code[2], code[1]); opbytes=3; break;
        case 0xcb: printf(" JMP, #$%02x%02x" , code[2], code[1]); opbytes=3; break;
        case 0xcc: printf(" CZ, #$%02x%02x" , code[2], code[1]); opbytes=3; break;
        case 0xcd: printf(" CALL, #$%02x%02x" , code[2], code[1]); opbytes=3; break;
        case 0xce: printf(" ACI, #$%02x" , code[1]); opbytes=2; break;
        case 0xcf: printf(" RST 1"); break;

        case 0xd0: printf(" RNC"); break;
        case 0xd1: printf(" POP D"); break;
        case 0xd2: printf(" JNC, #$%02x%02x" , code[2], code[1]); opbytes=3; break;
        case 0xd3: printf(" OUT, #$%02x" , code[1]); opbytes=2;  break;
        case 0xd4: printf(" CNC, #$%02x%02x" , code[2], code[1]); opbytes=3; break;
        case 0xd5: printf(" PUSH D"); break;
        case 0xd6: printf(" SUI, #$%02x" , code[1]); opbytes=2;  break;
        case 0xd7: printf(" RST 3"); break;
        case 0xd8: printf(" RC"); break;
        case 0xd9: printf(" RET"); break;
        case 0xda: printf(" JC, #$%02x%02x" , code[2], code[1]); opbytes=3; break;
        case 0xdb: printf(" IN, #$%02x" , code[1]); opbytes=2;  break;
        case 0xdc: printf(" CC, #$%02x%02x" , code[2], code[1]); opbytes=3; break;
        case 0xdd: printf(" CALL, #$%02x%02x" , code[2], code[1]); opbytes=3; break;
        case 0xde: printf(" SBI, #$%02x" , code[1]); opbytes=2; break;
        case 0xdf: printf(" RST 3"); break;

        case 0xe0: printf(" RPO"); break;
        case 0xe1: printf(" POP H"); break;
        case 0xe2: printf(" JPO, #$%02x%02x" , code[2], code[1]); opbytes=3; break;
        case 0xe3: printf(" XTHL"); break;
        case 0xe4: printf(" CPO, #$%02x%02x" , code[2], code[1]); opbytes=3; break;
        case 0xe5: printf(" PUSH H"); break;
        case 0xe6: printf(" ANI, #$%02x" , code[1]); opbytes=2;  break;
        case 0xe7: printf(" RST 4"); break;
        case 0xe8: printf(" RPE"); break;
        case 0xe9: printf(" PCHL"); break;
        case 0xea: printf(" JPE, #$%02x%02x" , code[2], code[1]); opbytes=3; break;
        case 0xeb: printf(" XCHG"); break;
        case 0xec: printf(" CPE, #$%02x%02x" , code[2], code[1]); opbytes=3; break;
        case 0xed: printf(" CALL, #$%02x%02x" , code[2], code[1]); opbytes=3; break;
        case 0xee: printf(" XRI, #$%02x" , code[1]); opbytes=2; break;
        case 0xef: printf(" RST 5"); break;

        case 0xf0: printf(" RP"); break;
        case 0xf1: printf(" POP PSW"); break;
        case 0xf2: printf(" JP, #$%02x%02x" , code[2], code[1]); opbytes=3; break;
        case 0xf3: printf(" DI"); break;
        case 0xf4: printf(" CP, #$%02x%02x" , code[2], code[1]); opbytes=3; break;
        case 0xf5: printf(" PUSH PSW"); break;
        case 0xf6: printf(" ORI, #$%02x" , code[1]); opbytes=2;  break;
        case 0xf7: printf(" RST 6"); break;
        case 0xf8: printf(" RM"); break;
        case 0xf9: printf(" SPHL"); break;
        case 0xfa: printf(" JM, #$%02x%02x" , code[2], code[1]); opbytes=3; break;
        case 0xfb: printf(" EI"); break;
        case 0xfc: printf(" CM, #$%02x%02x" , code[2], code[1]); opbytes=3; break;
        case 0xfd: printf(" CALL, #$%02x%02x" , code[2], code[1]); opbytes=3; break;
        case 0xfe: printf(" CPI, #$%02x" , code[1]); opbytes=2; break;
        case 0xff: printf(" RST 7"); break;
    }

    return opbytes;
}

typedef struct conditions {
    uint8_t z:1;
    uint8_t s:1;
    uint8_t p:1;
    uint8_t cy:1;
    uint8_t ac:1;
    uint8_t pad:1;
} conditions;

typedef struct currentState {
    uint8_t a;
    uint8_t b;
    uint8_t c;
    uint8_t d;
    uint8_t e;
    uint8_t h;
    uint8_t l;
    uint16_t sp;
    uint16_t pc;
    uint8_t *memory;
    struct conditions cc;
    uint8_t int_enable;
} currentState;

void unimplement(currentState* state) {
    printf("Error: Unimplemented intruction\n");
    exit(1);
}

int emulate(currentState* state) {
    unsigned char *opcode = state->memory[state->pc];

    switch(*opcode) {
        case 0x00: break;	//NOP
		case 0x01: 							//LXI	B,word
			state->c = opcode[1];
			state->b = opcode[2];
			state->pc += 2;
			break;
		case 0x02: unimplement(state); break;
		case 0x03: unimplement(state); break;
		case 0x04: unimplement(state); break;
		case 0x05: 							//DCR    B
			{
			uint8_t res = state->b - 1;
			state->cc.z = (res == 0);
			state->cc.s = (0x80 == (res & 0x80));
			state->cc.p = parity(res, 8);
			state->b = res;
			}
			break;
		case 0x06: 							//MVI B,byte
			state->b = opcode[1];
			state->pc++;
			break;
		case 0x07: unimplement(state); break;
		case 0x08: unimplement(state); break;
		case 0x09: 							//DAD B
			{
			uint32_t hl = (state->h << 8) | state->l;
			uint32_t bc = (state->b << 8) | state->c;
			uint32_t res = hl + bc;
			state->h = (res & 0xff00) >> 8;
			state->l = res & 0xff;
			state->cc.cy = ((res & 0xffff0000) > 0);
			}
			break;
		case 0x0a: unimplement(state); break;
		case 0x0b: unimplement(state); break;
		case 0x0c: unimplement(state); break;
		case 0x0d: 							//DCR    C
			{
			uint8_t res = state->c - 1;
			state->cc.z = (res == 0);
			state->cc.s = (0x80 == (res & 0x80));
			state->cc.p = parity(res, 8);
			state->c = res;
			}
			break;
		case 0x0e: 							//MVI C,byte
			state->c = opcode[1];
			state->pc++;
			break;
		case 0x0f: 							//RRC
			{
				uint8_t x = state->a;
				state->a = ((x & 1) << 7) | (x >> 1);
				state->cc.cy = (1 == (x&1));
			}
			break;
		case 0x10: unimplement(state); break;
		case 0x11: 							//LXI	D,word
			state->e = opcode[1];
			state->d = opcode[2];
			state->pc += 2;
			break;
		case 0x12: unimplement(state); break;
		case 0x13: 							//INX    D
			state->e++;
			if (state->e == 0)
				state->d++;
			break;		
		case 0x14: unimplement(state); break;
		case 0x15: unimplement(state); break;
		case 0x16: unimplement(state); break;
		case 0x17: unimplement(state); break;
		case 0x18: unimplement(state); break;
		case 0x19: 							//DAD    D
			{
			uint32_t hl = (state->h << 8) | state->l;
			uint32_t de = (state->d << 8) | state->e;
			uint32_t res = hl + de;
			state->h = (res & 0xff00) >> 8;
			state->l = res & 0xff;
			state->cc.cy = ((res & 0xffff0000) != 0);
			}
			break;
		case 0x1a: 							//LDAX	D
			{
			uint16_t offset=(state->d<<8) | state->e;
			state->a = state->memory[offset];
			}
			break;
		case 0x1b: unimplement(state); break;
		case 0x1c: unimplement(state); break;
		case 0x1d: unimplement(state); break;
		case 0x1e: unimplement(state); break;
		case 0x1f: unimplement(state); break;
		case 0x20: unimplement(state); break;
		case 0x21: 							//LXI	H,word
			state->l = opcode[1];
			state->h = opcode[2];
			state->pc += 2;
			break;
		case 0x22: unimplement(state); break;
		case 0x23: 							//INX    H
			state->l++;
			if (state->l == 0)
				state->h++;
			break;		
		case 0x24: unimplement(state); break;
		case 0x25: unimplement(state); break;
		case 0x26:  							//MVI H,byte
			state->h = opcode[1];
			state->pc++;
			break;
		case 0x27: unimplement(state); break;
		case 0x28: unimplement(state); break;
		case 0x29: 								//DAD    H
			{
			uint32_t hl = (state->h << 8) | state->l;
			uint32_t res = hl + hl;
			state->h = (res & 0xff00) >> 8;
			state->l = res & 0xff;
			state->cc.cy = ((res & 0xffff0000) != 0);
			}
			break;
		case 0x2a: unimplement(state); break;
		case 0x2b: unimplement(state); break;
		case 0x2c: unimplement(state); break;
		case 0x2d: unimplement(state); break;
		case 0x2e: unimplement(state); break;
		case 0x2f: unimplement(state); break;
		case 0x30: unimplement(state); break;
		case 0x31: 							//LXI	SP,word
			state->sp = (opcode[2]<<8) | opcode[1];
			state->pc += 2;
			break;
		case 0x32: 							//STA    (word)
			{
			uint16_t offset = (opcode[2]<<8) | (opcode[1]);
			state->memory[offset] = state->a;
			state->pc += 2;
			}
			break;
		case 0x33: unimplement(state); break;
		case 0x34: unimplement(state); break;
		case 0x35: unimplement(state); break;
		case 0x36: 							//MVI	M,byte
			{					
			//AC set if lower nibble of h was zero prior to dec
			uint16_t offset = (state->h<<8) | state->l;
			state->memory[offset] = opcode[1];
			state->pc++;
			}
			break;
		case 0x37: unimplement(state); break;
		case 0x38: unimplement(state); break;
		case 0x39: unimplement(state); break;
		case 0x3a: 							//LDA    (word)
			{
			uint16_t offset = (opcode[2]<<8) | (opcode[1]);
			state->a = state->memory[offset];
			state->pc+=2;
			}
			break;
		case 0x3b: unimplement(state); break;
		case 0x3c: unimplement(state); break;
		case 0x3d: unimplement(state); break;
		case 0x3e: 							//MVI    A,byte
			state->a = opcode[1];
			state->pc++;
			break;
		case 0x3f: unimplement(state); break;
		case 0x40: unimplement(state); break;
		case 0x41: unimplement(state); break;
		case 0x42: unimplement(state); break;
		case 0x43: unimplement(state); break;
		case 0x44: unimplement(state); break;
		case 0x45: unimplement(state); break;
		case 0x46: unimplement(state); break;
		case 0x47: unimplement(state); break;
		case 0x48: unimplement(state); break;
		case 0x49: unimplement(state); break;
		case 0x4a: unimplement(state); break;
		case 0x4b: unimplement(state); break;
		case 0x4c: unimplement(state); break;
		case 0x4d: unimplement(state); break;
		case 0x4e: unimplement(state); break;
		case 0x4f: unimplement(state); break;
		case 0x50: unimplement(state); break;
		case 0x51: unimplement(state); break;
		case 0x52: unimplement(state); break;
		case 0x53: unimplement(state); break;
		case 0x54: unimplement(state); break;
		case 0x55: unimplement(state); break;
		case 0x56: 							//MOV D,M
			{
			uint16_t offset = (state->h<<8) | (state->l);
			state->d = state->memory[offset];
			}
			break;
		case 0x57: unimplement(state); break;
		case 0x58: unimplement(state); break;
		case 0x59: unimplement(state); break;
		case 0x5a: unimplement(state); break;
		case 0x5b: unimplement(state); break;
		case 0x5c: unimplement(state); break;
		case 0x5d: unimplement(state); break;
		case 0x5e: 							//MOV E,M
			{
			uint16_t offset = (state->h<<8) | (state->l);
			state->e = state->memory[offset];
			}
			break;
		case 0x5f: unimplement(state); break;
		case 0x60: unimplement(state); break;
		case 0x61: unimplement(state); break;
		case 0x62: unimplement(state); break;
		case 0x63: unimplement(state); break;
		case 0x64: unimplement(state); break;
		case 0x65: unimplement(state); break;
		case 0x66: 							//MOV H,M
			{
			uint16_t offset = (state->h<<8) | (state->l);
			state->h = state->memory[offset];
			}
			break;
		case 0x67: unimplement(state); break;
		case 0x68: unimplement(state); break;
		case 0x69: unimplement(state); break;
		case 0x6a: unimplement(state); break;
		case 0x6b: unimplement(state); break;
		case 0x6c: unimplement(state); break;
		case 0x6d: unimplement(state); break;
		case 0x6e: unimplement(state); break;
		case 0x6f: state->l = state->a; break; //MOV L,A
		case 0x70: unimplement(state); break;
		case 0x71: unimplement(state); break;
		case 0x72: unimplement(state); break;
		case 0x73: unimplement(state); break;
		case 0x74: unimplement(state); break;
		case 0x75: unimplement(state); break;
		case 0x76: unimplement(state); break;
		case 0x77: 							//MOV    M,A
			{
			uint16_t offset = (state->h<<8) | (state->l);
			state->memory[offset] = state->a;
			}
			break;
		case 0x78: unimplement(state); break;
		case 0x79: unimplement(state); break;
		case 0x7a: state->a  = state->d;  break;	//MOV D,A
		case 0x7b: state->a  = state->e;  break;	//MOV E,A
		case 0x7c: state->a  = state->h;  break;	//MOV H,A
		case 0x7d: unimplement(state); break;
		case 0x7e: 							//MOV A,M
			{
			uint16_t offset = (state->h<<8) | (state->l);
			state->a = state->memory[offset];
			}
			break;
		case 0x7f: unimplement(state); break;
		case 0x80: unimplement(state); break;
		case 0x81: unimplement(state); break;
		case 0x82: unimplement(state); break;
		case 0x83: unimplement(state); break;
		case 0x84: unimplement(state); break;
		case 0x85: unimplement(state); break;
		case 0x86: unimplement(state); break;
		case 0x87: unimplement(state); break;
		case 0x88: unimplement(state); break;
		case 0x89: unimplement(state); break;
		case 0x8a: unimplement(state); break;
		case 0x8b: unimplement(state); break;
		case 0x8c: unimplement(state); break;
		case 0x8d: unimplement(state); break;
		case 0x8e: unimplement(state); break;
		case 0x8f: unimplement(state); break;
		case 0x90: unimplement(state); break;
		case 0x91: unimplement(state); break;
		case 0x92: unimplement(state); break;
		case 0x93: unimplement(state); break;
		case 0x94: unimplement(state); break;
		case 0x95: unimplement(state); break;
		case 0x96: unimplement(state); break;
		case 0x97: unimplement(state); break;
		case 0x98: unimplement(state); break;
		case 0x99: unimplement(state); break;
		case 0x9a: unimplement(state); break;
		case 0x9b: unimplement(state); break;
		case 0x9c: unimplement(state); break;
		case 0x9d: unimplement(state); break;
		case 0x9e: unimplement(state); break;
		case 0x9f: unimplement(state); break;
		case 0xa0: unimplement(state); break;
		case 0xa1: unimplement(state); break;
		case 0xa2: unimplement(state); break;
		case 0xa3: unimplement(state); break;
		case 0xa4: unimplement(state); break;
		case 0xa5: unimplement(state); break;
		case 0xa6: unimplement(state); break;
		case 0xa7: state->a = state->a & state->a; LogicFlagsA(state);	break; //ANA A
		case 0xa8: unimplement(state); break;
		case 0xa9: unimplement(state); break;
		case 0xaa: unimplement(state); break;
		case 0xab: unimplement(state); break;
		case 0xac: unimplement(state); break;
		case 0xad: unimplement(state); break;
		case 0xae: unimplement(state); break;
		case 0xaf: state->a = state->a ^ state->a; LogicFlagsA(state);	break; //XRA A
		case 0xb0: unimplement(state); break;
		case 0xb1: unimplement(state); break;
		case 0xb2: unimplement(state); break;
		case 0xb3: unimplement(state); break;
		case 0xb4: unimplement(state); break;
		case 0xb5: unimplement(state); break;
		case 0xb6: unimplement(state); break;
		case 0xb7: unimplement(state); break;
		case 0xb8: unimplement(state); break;
		case 0xb9: unimplement(state); break;
		case 0xba: unimplement(state); break;
		case 0xbb: unimplement(state); break;
		case 0xbc: unimplement(state); break;
		case 0xbd: unimplement(state); break;
		case 0xbe: unimplement(state); break;
		case 0xbf: unimplement(state); break;
		case 0xc0: unimplement(state); break;
		case 0xc1: 						//POP    B
			{
				state->c = state->memory[state->sp];
				state->b = state->memory[state->sp+1];
				state->sp += 2;
			}
			break;
		case 0xc2: 						//JNZ address
			if (0 == state->cc.z)
				state->pc = (opcode[2] << 8) | opcode[1];
			else
				state->pc += 2;
			break;
		case 0xc3:						//JMP address
			state->pc = (opcode[2] << 8) | opcode[1];
			break;
		case 0xc4: unimplement(state); break;
		case 0xc5: 						//PUSH   B
			{
			state->memory[state->sp-1] = state->b;
			state->memory[state->sp-2] = state->c;
			state->sp = state->sp - 2;
			}
			break;
		case 0xc6: 						//ADI    byte
			{
			uint16_t x = (uint16_t) state->a + (uint16_t) opcode[1];
			state->cc.z = ((x & 0xff) == 0);
			state->cc.s = (0x80 == (x & 0x80));
			state->cc.p = parity((x&0xff), 8);
			state->cc.cy = (x > 0xff);
			state->a = (uint8_t) x;
			state->pc++;
			}
			break;
		case 0xc7: unimplement(state); break;
		case 0xc8: unimplement(state); break;
		case 0xc9: 						//RET
			state->pc = state->memory[state->sp] | (state->memory[state->sp+1] << 8);
			state->sp += 2;
			break;
		case 0xca: unimplement(state); break;
		case 0xcb: unimplement(state); break;
		case 0xcc: unimplement(state); break;
		case 0xcd: 						//CALL adr
			{
			uint16_t	ret = state->pc+2;
			state->memory[state->sp-1] = (ret >> 8) & 0xff;
			state->memory[state->sp-2] = (ret & 0xff);
			state->sp = state->sp - 2;
			state->pc = (opcode[2] << 8) | opcode[1];
			}
 			break;
		case 0xce: unimplement(state); break;
		case 0xcf: unimplement(state); break;
		case 0xd0: unimplement(state); break;
		case 0xd1: 						//POP    D
			{
				state->e = state->memory[state->sp];
				state->d = state->memory[state->sp+1];
				state->sp += 2;
			}
			break;
		case 0xd2: unimplement(state); break;
		case 0xd3: 
			//Don't know what to do here (yet)
			state->pc++;
			break;
		case 0xd4: unimplement(state); break;
		case 0xd5: 						//PUSH   D
			{
			state->memory[state->sp-1] = state->d;
			state->memory[state->sp-2] = state->e;
			state->sp = state->sp - 2;
			}
			break;
		case 0xd6: unimplement(state); break;
		case 0xd7: unimplement(state); break;
		case 0xd8: unimplement(state); break;
		case 0xd9: unimplement(state); break;
		case 0xda: unimplement(state); break;
		case 0xdb: unimplement(state); break;
		case 0xdc: unimplement(state); break;
		case 0xdd: unimplement(state); break;
		case 0xde: unimplement(state); break;
		case 0xdf: unimplement(state); break;
		case 0xe0: unimplement(state); break;
		case 0xe1: 					//POP    H
			{
				state->l = state->memory[state->sp];
				state->h = state->memory[state->sp+1];
				state->sp += 2;
			}
			break;
		case 0xe2: unimplement(state); break;
		case 0xe3: unimplement(state); break;
		case 0xe4: unimplement(state); break;
		case 0xe5: 						//PUSH   H
			{
			state->memory[state->sp-1] = state->h;
			state->memory[state->sp-2] = state->l;
			state->sp = state->sp - 2;
			}
			break;
		case 0xe6: 						//ANI    byte
			{
			state->a = state->a & opcode[1];
			LogicFlagsA(state);
			state->pc++;
			}
			break;
		case 0xe7: unimplement(state); break;
		case 0xe8: unimplement(state); break;
		case 0xe9: unimplement(state); break;
		case 0xea: unimplement(state); break;
		case 0xeb: 					//XCHG
			{
				uint8_t save1 = state->d;
				uint8_t save2 = state->e;
				state->d = state->h;
				state->e = state->l;
				state->h = save1;
				state->l = save2;
			}
			break;
		case 0xec: unimplement(state); break;
		case 0xed: unimplement(state); break;
		case 0xee: unimplement(state); break;
		case 0xef: unimplement(state); break;
		case 0xf0: unimplement(state); break;
		case 0xf1: 					//POP PSW
			{
				state->a = state->memory[state->sp+1];
				uint8_t psw = state->memory[state->sp];
				state->cc.z  = (0x01 == (psw & 0x01));
				state->cc.s  = (0x02 == (psw & 0x02));
				state->cc.p  = (0x04 == (psw & 0x04));
				state->cc.cy = (0x05 == (psw & 0x08));
				state->cc.ac = (0x10 == (psw & 0x10));
				state->sp += 2;
			}
			break;
		case 0xf2: unimplement(state); break;
		case 0xf3: unimplement(state); break;
		case 0xf4: unimplement(state); break;
		case 0xf5: 						//PUSH   PSW
			{
			state->memory[state->sp-1] = state->a;
			uint8_t psw = (state->cc.z |
							state->cc.s << 1 |
							state->cc.p << 2 |
							state->cc.cy << 3 |
							state->cc.ac << 4 );
			state->memory[state->sp-2] = psw;
			state->sp = state->sp - 2;
			}
			break;
		case 0xf6: unimplement(state); break;
		case 0xf7: unimplement(state); break;
		case 0xf8: unimplement(state); break;
		case 0xf9: unimplement(state); break;
		case 0xfa: unimplement(state); break;
		case 0xfb: state->int_enable = 1;  break;	//EI
		case 0xfc: unimplement(state); break;
		case 0xfd: unimplement(state); break;
		case 0xfe: 						//CPI  byte
			{
			uint8_t x = state->a - opcode[1];
			state->cc.z = (x == 0);
			state->cc.s = (0x80 == (x & 0x80));
			state->cc.p = parity(x, 8);
			state->cc.cy = (state->a < opcode[1]);
			state->pc++;
			}
			break;
		case 0xff: unimplement(state); break;
    }
    printf("\t");
	printf("%c", state->cc.z ? 'z' : '.');
	printf("%c", state->cc.s ? 's' : '.');
	printf("%c", state->cc.p ? 'p' : '.');
	printf("%c", state->cc.cy ? 'c' : '.');
	printf("%c  ", state->cc.ac ? 'a' : '.');
	printf("A $%02x B $%02x C $%02x D $%02x E $%02x H $%02x L $%02x SP %04x\n", state->a, state->b, state->c,
				state->d, state->e, state->h, state->l, state->sp);
	return 0;
}

int main (int argc, char**argv) {    
    FILE *f= fopen(argv[1], "rb");    
    if (f==NULL) {    
        printf("error: Couldn't open %s\n" , argv[1]);    
        exit(1);    
    }    

    fseek(f, 0L, SEEK_END);    
    int fsize = ftell(f);    
    fseek(f, 0L, SEEK_SET);    

    unsigned char *buffer=malloc(fsize);    

    fread(buffer, fsize, 1, f);    
    fclose(f);    

    int pc = 0;    

    while (pc < fsize) {    
        pc += disassemble(buffer, pc);    
    }    
    return 0;    
} 