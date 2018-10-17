from .. import wire
from .. import gate
from .. import signal
from . import ADD

Wire = wire.Wire
Bus4 = wire.Bus4
Bus8 = wire.Bus8
Bus16 = wire.Bus16


class AdderSubtractor4:
    def __init__(
            self,
            add_subtract,
            input_bus_1,
            input_bus_2,
            overflow,
            carry_out,
            output_bus
            ):
        if len(input_bus_1.wires) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(input_bus_1.wires)
                )
            )

        if len(input_bus_2.wires) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(input_bus_2.wires)
                )
            )

        if len(output_bus.wires) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(output_bus.wires)
                )
            )

        wire_1 = Wire()
        wire_2 = Wire()
        wire_3 = Wire()
        wire_4 = Wire()
        not_input_1 = Wire()
        not_input_2 = Wire()
        not_output = Wire()
        and_1_wire = Wire()
        and_2_wire = Wire()
        bus_1 = Bus4(wire_1, wire_2, wire_3, wire_4)
        input_1 = input_bus_1.wires
        input_2 = input_bus_2.wires
        output = output_bus.wires

        signal.ControlledInverter4(add_subtract, input_bus_2, bus_1)
        ADD.Adder4(add_subtract, input_bus_1, bus_1, carry_out, output_bus)

        gate.NOTGate(input_1[0], not_input_1)
        gate.NOTGate(input_2[0], not_input_2)
        gate.NOTGate(output[0], not_output)
        gate.ANDGate3(input_1[0], not_input_2, not_output, and_1_wire)
        gate.ANDGate3(not_input_1, input_2[0], output[0], and_2_wire)
        gate.ORGate2(and_1_wire, and_2_wire, overflow)


class AdderSubtractor8:
    def __init__(
            self,
            add_subtract,
            input_bus_1,
            input_bus_2,
            overflow,
            carry_out,
            output_bus
            ):
        if len(input_bus_1.wires) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(input_bus_1.wires)
                )
            )

        if len(input_bus_2.wires) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(input_bus_2.wires)
                )
            )

        if len(output_bus.wires) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(output_bus.wires)
                )
            )

        wire_1 = Wire()
        wire_2 = Wire()
        wire_3 = Wire()
        wire_4 = Wire()
        wire_5 = Wire()
        wire_6 = Wire()
        wire_7 = Wire()
        wire_8 = Wire()
        not_input_1 = Wire()
        not_input_2 = Wire()
        not_output = Wire()
        and_1_wire = Wire()
        and_2_wire = Wire()
        bus_1 = Bus8(
            wire_1,
            wire_2,
            wire_3,
            wire_4,
            wire_5,
            wire_6,
            wire_7,
            wire_8
        )
        input_1 = input_bus_1.wires
        input_2 = input_bus_2.wires
        output = output_bus.wires

        signal.ControlledInverter8(add_subtract, input_bus_2, bus_1)
        ADD.Adder8(add_subtract, input_bus_1, bus_1, carry_out, output_bus)

        gate.NOTGate(input_1[0], not_input_1)
        gate.NOTGate(input_2[0], not_input_2)
        gate.NOTGate(output[0], not_output)
        gate.ANDGate3(input_1[0], not_input_2, not_output, and_1_wire)
        gate.ANDGate3(not_input_1, input_2[0], output[0], and_2_wire)
        gate.ORGate2(and_1_wire, and_2_wire, overflow)


class AdderSubtractor16:
    def __init__(
            self,
            add_subtract,
            input_bus_1,
            input_bus_2,
            overflow,
            carry_out,
            output_bus
            ):
        if len(input_bus_1.wires) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(input_bus_1.wires)
                )
            )

        if len(input_bus_2.wires) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(input_bus_2.wires)
                )
            )

        if len(output_bus.wires) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(output_bus.wires)
                )
            )

        wire_1 = Wire()
        wire_2 = Wire()
        wire_3 = Wire()
        wire_4 = Wire()
        wire_5 = Wire()
        wire_6 = Wire()
        wire_7 = Wire()
        wire_8 = Wire()
        wire_9 = Wire()
        wire_10 = Wire()
        wire_11 = Wire()
        wire_12 = Wire()
        wire_13 = Wire()
        wire_14 = Wire()
        wire_15 = Wire()
        wire_16 = Wire()
        not_input_1 = Wire()
        not_input_2 = Wire()
        not_output = Wire()
        and_1_wire = Wire()
        and_2_wire = Wire()
        bus_1 = Bus16(
            wire_1,
            wire_2,
            wire_3,
            wire_4,
            wire_5,
            wire_6,
            wire_7,
            wire_8,
            wire_9,
            wire_10,
            wire_11,
            wire_12,
            wire_13,
            wire_14,
            wire_15,
            wire_16
        )
        input_1 = input_bus_1.wires
        input_2 = input_bus_2.wires
        output = output_bus.wires

        signal.ControlledInverter16(add_subtract, input_bus_2, bus_1)
        ADD.Adder16(add_subtract, input_bus_1, bus_1, carry_out, output_bus)

        gate.NOTGate(input_1[0], not_input_1)
        gate.NOTGate(input_2[0], not_input_2)
        gate.NOTGate(output[0], not_output)
        gate.ANDGate3(input_1[0], not_input_2, not_output, and_1_wire)
        gate.ANDGate3(not_input_1, input_2[0], output[0], and_2_wire)
        gate.ORGate2(and_1_wire, and_2_wire, overflow)
