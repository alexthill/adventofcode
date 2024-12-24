def get_gate_out(gates, in1, op, in2):
    out = gates.get((in1, op, in2))
    if out is None:
        out = gates.get((in2, op, in1))
    return out

def day24(file):
    inputs = {}
    for line in file:
        if line == "\n":
            break
        wire, value = line.split(": ")
        inputs[wire] = int(value)

    gates = {}
    for line in file:
        if line == "\n":
            continue
        gate, output = line.split(" -> ")
        in1, op, in2 = gate.split(" ")
        gates[(in1, op, in2)] = output[:len(output) - 1]
    print(len(gates))
    gates2 = gates.copy()

    while True:
        founds = []
        for gate, output in gates.items():
            in1, op, in2 = gate
            if in1 in inputs and in2 in inputs:
                founds.append(gate)
                if op == "AND":
                    inputs[output] = inputs[in1] & inputs[in2]
                elif op == "OR":
                    inputs[output] = inputs[in1] | inputs[in2]
                else:
                    inputs[output] = inputs[in1] ^ inputs[in2]
        if len(founds) == 0:
            break
        for found in founds:
            del gates[found]

    zs = []
    for wire, value in inputs.items():
        if wire[0] == "z":
            zs.append((wire, value))
    zs.sort()
    sol1 = 0
    for wire, value in reversed(zs):
        sol1 = sol1 * 2 + value

    gates = gates2
    print(f"==== adder 00 ====")
    xor1 = get_gate_out(gates, "x00", "XOR", "y00")
    carry = get_gate_out(gates, "x00", "AND", "y00")
    print("x xor y = sum =", xor1)
    print("next carry =", carry)
    for i in range(1, 45):
        print(f"==== adder {i:03d} ====")
        xor1 = get_gate_out(gates, f"x{i:02d}", "XOR", f"y{i:02d}")
        xor2 = get_gate_out(gates, xor1, "XOR", carry)
        print("x xor y =", xor1)
        print("(x xor y) xor carry = sum =", xor2)
        and1 = get_gate_out(gates, f"x{i:02d}", "AND", f"y{i:02d}")
        and2 = get_gate_out(gates, xor1, "AND", carry)
        print("x and y =", and1)
        print("(x xor y) and carry =", and2)
        carry = get_gate_out(gates, and1, "OR", and2)
        print("next carry =", carry)
    print("")
    print("These are the outputs for all the gates in every full adder, now go and figure out what is wrong")
    print("Hint: Find the first None, fix the wires that cause it and repeat")
    print("")

    # yay hardcoded solution that ony works for my input
    return (sol1, "gbf,hdt,jgt,mht,nbf,z05,z09,z30")
