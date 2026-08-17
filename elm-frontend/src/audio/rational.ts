export type WireNumber = number | { num: number; den: number };

function gcd(a: bigint, b: bigint): bigint {
    a = a < 0n ? -a : a;
    b = b < 0n ? -b : b;
    while (b) {
        [a, b] = [b, a % b];
    }
    return a === 0n ? 1n : a;
}

export class Rational {
    static readonly ZERO = new Rational(0n, 1n);

    readonly num: bigint;
    readonly den: bigint;

    constructor(num: bigint, den: bigint = 1n) {
        if (den < 0n) {
            num = -num;
            den = -den;
        }
        if (num === 0n) {
            this.num = 0n;
            this.den = 1n;
            return;
        }
        const g = gcd(num, den);
        this.num = num / g;
        this.den = den / g;
    }

    static from_wire(value: WireNumber): Rational {
        return typeof value === 'number'
            ? new Rational(BigInt(Math.round(value)), 1n)
            : new Rational(BigInt(value.num), BigInt(value.den));
    }

    static from_int(n: number): Rational {
        return new Rational(BigInt(n), 1n);
    }

    add(other: Rational): Rational {
        return new Rational(this.num * other.den + other.num * this.den, this.den * other.den);
    }

    sub(other: Rational): Rational {
        return new Rational(this.num * other.den - other.num * this.den, this.den * other.den);
    }

    mul(other: Rational): Rational {
        return new Rational(this.num * other.num, this.den * other.den);
    }

    div(other: Rational): Rational {
        if (other.num === 0n) throw new Error('division by zero');
        return new Rational(this.num * other.den, this.den * other.num);
    }

    toNumber(): number {
        return Number(this.num) / Number(this.den);
    }

    isZero(): boolean {
        return this.num === 0n;
    }

    lessThan(other: Rational): boolean {
        return this.num * other.den < other.num * this.den;
    }
}

export function wire_to_number(value: WireNumber): number {
    return typeof value === 'number' ? value : value.num / value.den;
}

export function to_seconds(dur: Rational, bpm: number): number {
    return (60 * dur.toNumber()) / bpm;
}
