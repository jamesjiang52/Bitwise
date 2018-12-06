"""
The following classes are defined:
    Multiplier2
    Multiplier4
    Multiplier8
"""

from .. import wire
from . import gate
from . import ADD

Wire = wire.Wire
Bus4 = wire.Bus4
Bus8 = wire.Bus8
Bus16 = wire.Bus16


class Multiplier2:
    """Construct a new 2-bit unsigned multiplier.

    Args:
        a_1: An object of type Wire. The most significant bit of the
            multiplicand.
        a_2: An object of type Wire. The least significant bit of the
            multiplicand.
        b_1: An object of type Wire. The most significant bit of the
            multiplier.
        b_2: An object of type Wire. The least significant bit of the
            multiplier.
        product_bus: An object of type Bus4. The product. product_bus[0] and
            product_bus[3] are the most and least significant bit,
            respectively.

    Raises:
        TypeError: If product_bus is not a bus of width 4.
    """
    def __init__(self, a_1, a_2, b_1, b_2, product_bus):
        if len(product_bus) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(product_bus)
                )
            )

        and_1 = Wire()
        and_3 = Wire()
        and_4 = Wire()
        cout_1 = Wire()
        gnd = Wire()
        gnd.value = 0

        gate.ANDGate2(a_1, b_2, and_1)
        gate.ANDGate2(a_2, b_2, product_bus[3])
        gate.ANDGate2(a_1, b_1, and_3)
        gate.ANDGate2(a_2, b_1, and_4)

        ADD.FullAdder(gnd, and_1, and_4, cout_1, product_bus[2])
        ADD.FullAdder(cout_1, gnd, and_3, product_bus[0], product_bus[1])


class Multiplier4:
    """Construct a new 4-bit unsigned multiplier.

    Args:
        a_bus: An object of type Bus4. The multiplicand. a_bus[0] and a_bus[3]
            are the most and least significant bit, respectively.
        b_bus: An object of type Bus4. The multiplier. b_bus[0] and b_bus[3]
            are the most and least significant bit, respectively.
        product_bus: An object of type Bus8. The product. product_bus[0] and
            product_bus[7] are the most and least significant bit,
            respectively.

    Raises:
        TypeError: If either a_bus or b_bus is not a bus of width 4, or if
            product_bus is not a bus of width 8.
    """
    def __init__(self, a_bus, b_bus, product_bus):
        if len(a_bus) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(a_bus)
                )
            )

        if len(b_bus) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(b_bus)
                )
            )

        if len(product_bus) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(product_bus)
                )
            )

        and_3_0 = Wire()
        and_2_0 = Wire()
        and_1_0 = Wire()
        and_3_1 = Wire()
        and_2_1 = Wire()
        and_1_1 = Wire()
        and_0_1 = Wire()
        and_3_2 = Wire()
        and_2_2 = Wire()
        and_1_2 = Wire()
        and_0_2 = Wire()
        and_3_3 = Wire()
        and_2_3 = Wire()
        and_1_3 = Wire()
        and_0_3 = Wire()
        gnd = Wire()
        gnd.value = 0

        cout_1 = Wire()
        add_1_1 = Wire()
        add_1_2 = Wire()
        add_1_3 = Wire()
        cout_2 = Wire()
        add_2_1 = Wire()
        add_2_2 = Wire()
        add_2_3 = Wire()

        gate.ANDGate2(a_bus[0], b_bus[3], and_3_0)
        gate.ANDGate2(a_bus[1], b_bus[3], and_2_0)
        gate.ANDGate2(a_bus[2], b_bus[3], and_1_0)
        gate.ANDGate2(a_bus[3], b_bus[3], product_bus[7])
        gate.ANDGate2(a_bus[0], b_bus[2], and_3_1)
        gate.ANDGate2(a_bus[1], b_bus[2], and_2_1)
        gate.ANDGate2(a_bus[2], b_bus[2], and_1_1)
        gate.ANDGate2(a_bus[3], b_bus[2], and_0_1)
        gate.ANDGate2(a_bus[0], b_bus[1], and_3_2)
        gate.ANDGate2(a_bus[1], b_bus[1], and_2_2)
        gate.ANDGate2(a_bus[2], b_bus[1], and_1_2)
        gate.ANDGate2(a_bus[3], b_bus[1], and_0_2)
        gate.ANDGate2(a_bus[0], b_bus[0], and_3_3)
        gate.ANDGate2(a_bus[1], b_bus[0], and_2_3)
        gate.ANDGate2(a_bus[2], b_bus[0], and_1_3)
        gate.ANDGate2(a_bus[3], b_bus[0], and_0_3)

        bus_1_1 = Bus4(and_3_1, and_2_1, and_1_1, and_0_1)
        bus_1_2 = Bus4(gnd, and_3_0, and_2_0, and_1_0)
        bus_1_out = Bus4(add_1_1, add_1_2, add_1_3, product_bus[6])

        bus_2_1 = Bus4(and_3_2, and_2_2, and_1_2, and_0_2)
        bus_2_2 = Bus4(cout_1, add_1_1, add_1_2, add_1_3)
        bus_2_out = Bus4(add_2_1, add_2_2, add_2_3, product_bus[5])

        bus_3_1 = Bus4(and_3_3, and_2_3, and_1_3, and_0_3)
        bus_3_2 = Bus4(cout_2, add_2_1, add_2_2, add_2_3)
        bus_3_out = Bus4(*product_bus[1:5])

        ADD.Adder4(gnd, bus_1_1, bus_1_2, cout_1, bus_1_out)
        ADD.Adder4(gnd, bus_2_1, bus_2_2, cout_2, bus_2_out)
        ADD.Adder4(gnd, bus_3_1, bus_3_2, product_bus[0], bus_3_out)


class Multiplier8:
    """Construct a new 8-bit unsigned multiplier.

    Args:
        a_bus: An object of type Bus8. The multiplicand. a_bus[0] and a_bus[7]
            are the most and least significant bit, respectively.
        b_bus: An object of type Bus8. The multiplier. b_bus[0] and b_bus[7]
            are the most and least significant bit, respectively.
        product_bus: An object of type Bus16. The product. product_bus[0] and
            product_bus[15] are the most and least significant bit,
            respectively.

    Raises:
        TypeError: If either a_bus or b_bus is not a bus of width 8, or if
            product_bus is not a bus of width 16.
    """
    def __init__(self, a_bus, b_bus, product_bus):
        if len(a_bus) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(a_bus)
                )
            )

        if len(b_bus) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(b_bus)
                )
            )

        if len(product_bus) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(product_bus)
                )
            )

        and_7_0 = Wire()
        and_6_0 = Wire()
        and_5_0 = Wire()
        and_4_0 = Wire()
        and_3_0 = Wire()
        and_2_0 = Wire()
        and_1_0 = Wire()
        and_7_1 = Wire()
        and_6_1 = Wire()
        and_5_1 = Wire()
        and_4_1 = Wire()
        and_3_1 = Wire()
        and_2_1 = Wire()
        and_1_1 = Wire()
        and_0_1 = Wire()
        and_7_2 = Wire()
        and_6_2 = Wire()
        and_5_2 = Wire()
        and_4_2 = Wire()
        and_3_2 = Wire()
        and_2_2 = Wire()
        and_1_2 = Wire()
        and_0_2 = Wire()
        and_7_3 = Wire()
        and_6_3 = Wire()
        and_5_3 = Wire()
        and_4_3 = Wire()
        and_3_3 = Wire()
        and_2_3 = Wire()
        and_1_3 = Wire()
        and_0_3 = Wire()
        and_7_4 = Wire()
        and_6_4 = Wire()
        and_5_4 = Wire()
        and_4_4 = Wire()
        and_3_4 = Wire()
        and_2_4 = Wire()
        and_1_4 = Wire()
        and_0_4 = Wire()
        and_7_5 = Wire()
        and_6_5 = Wire()
        and_5_5 = Wire()
        and_4_5 = Wire()
        and_3_5 = Wire()
        and_2_5 = Wire()
        and_1_5 = Wire()
        and_0_5 = Wire()
        and_7_6 = Wire()
        and_6_6 = Wire()
        and_5_6 = Wire()
        and_4_6 = Wire()
        and_3_6 = Wire()
        and_2_6 = Wire()
        and_1_6 = Wire()
        and_0_6 = Wire()
        and_7_7 = Wire()
        and_6_7 = Wire()
        and_5_7 = Wire()
        and_4_7 = Wire()
        and_3_7 = Wire()
        and_2_7 = Wire()
        and_1_7 = Wire()
        and_0_7 = Wire()
        gnd = Wire()
        gnd.value = 0

        cout_1 = Wire()
        add_1_1 = Wire()
        add_1_2 = Wire()
        add_1_3 = Wire()
        add_1_4 = Wire()
        add_1_5 = Wire()
        add_1_6 = Wire()
        add_1_7 = Wire()
        cout_2 = Wire()
        add_2_1 = Wire()
        add_2_2 = Wire()
        add_2_3 = Wire()
        add_2_4 = Wire()
        add_2_5 = Wire()
        add_2_6 = Wire()
        add_2_7 = Wire()
        cout_3 = Wire()
        add_3_1 = Wire()
        add_3_2 = Wire()
        add_3_3 = Wire()
        add_3_4 = Wire()
        add_3_5 = Wire()
        add_3_6 = Wire()
        add_3_7 = Wire()
        cout_4 = Wire()
        add_4_1 = Wire()
        add_4_2 = Wire()
        add_4_3 = Wire()
        add_4_4 = Wire()
        add_4_5 = Wire()
        add_4_6 = Wire()
        add_4_7 = Wire()
        cout_5 = Wire()
        add_5_1 = Wire()
        add_5_2 = Wire()
        add_5_3 = Wire()
        add_5_4 = Wire()
        add_5_5 = Wire()
        add_5_6 = Wire()
        add_5_7 = Wire()
        cout_6 = Wire()
        add_6_1 = Wire()
        add_6_2 = Wire()
        add_6_3 = Wire()
        add_6_4 = Wire()
        add_6_5 = Wire()
        add_6_6 = Wire()
        add_6_7 = Wire()

        gate.ANDGate2(a_bus[0], b_bus[7], and_7_0)
        gate.ANDGate2(a_bus[1], b_bus[7], and_6_0)
        gate.ANDGate2(a_bus[2], b_bus[7], and_5_0)
        gate.ANDGate2(a_bus[3], b_bus[7], and_4_0)
        gate.ANDGate2(a_bus[4], b_bus[7], and_3_0)
        gate.ANDGate2(a_bus[5], b_bus[7], and_2_0)
        gate.ANDGate2(a_bus[6], b_bus[7], and_1_0)
        gate.ANDGate2(a_bus[7], b_bus[7], product_bus[15])

        gate.ANDGate2(a_bus[0], b_bus[6], and_7_1)
        gate.ANDGate2(a_bus[1], b_bus[6], and_6_1)
        gate.ANDGate2(a_bus[2], b_bus[6], and_5_1)
        gate.ANDGate2(a_bus[3], b_bus[6], and_4_1)
        gate.ANDGate2(a_bus[4], b_bus[6], and_3_1)
        gate.ANDGate2(a_bus[5], b_bus[6], and_2_1)
        gate.ANDGate2(a_bus[6], b_bus[6], and_1_1)
        gate.ANDGate2(a_bus[7], b_bus[6], and_0_1)

        gate.ANDGate2(a_bus[0], b_bus[5], and_7_2)
        gate.ANDGate2(a_bus[1], b_bus[5], and_6_2)
        gate.ANDGate2(a_bus[2], b_bus[5], and_5_2)
        gate.ANDGate2(a_bus[3], b_bus[5], and_4_2)
        gate.ANDGate2(a_bus[4], b_bus[5], and_3_2)
        gate.ANDGate2(a_bus[5], b_bus[5], and_2_2)
        gate.ANDGate2(a_bus[6], b_bus[5], and_1_2)
        gate.ANDGate2(a_bus[7], b_bus[5], and_0_2)

        gate.ANDGate2(a_bus[0], b_bus[4], and_7_3)
        gate.ANDGate2(a_bus[1], b_bus[4], and_6_3)
        gate.ANDGate2(a_bus[2], b_bus[4], and_5_3)
        gate.ANDGate2(a_bus[3], b_bus[4], and_4_3)
        gate.ANDGate2(a_bus[4], b_bus[4], and_3_3)
        gate.ANDGate2(a_bus[5], b_bus[4], and_2_3)
        gate.ANDGate2(a_bus[6], b_bus[4], and_1_3)
        gate.ANDGate2(a_bus[7], b_bus[4], and_0_3)

        gate.ANDGate2(a_bus[0], b_bus[3], and_7_4)
        gate.ANDGate2(a_bus[1], b_bus[3], and_6_4)
        gate.ANDGate2(a_bus[2], b_bus[3], and_5_4)
        gate.ANDGate2(a_bus[3], b_bus[3], and_4_4)
        gate.ANDGate2(a_bus[4], b_bus[3], and_3_4)
        gate.ANDGate2(a_bus[5], b_bus[3], and_2_4)
        gate.ANDGate2(a_bus[6], b_bus[3], and_1_4)
        gate.ANDGate2(a_bus[7], b_bus[3], and_0_4)

        gate.ANDGate2(a_bus[0], b_bus[2], and_7_5)
        gate.ANDGate2(a_bus[1], b_bus[2], and_6_5)
        gate.ANDGate2(a_bus[2], b_bus[2], and_5_5)
        gate.ANDGate2(a_bus[3], b_bus[2], and_4_5)
        gate.ANDGate2(a_bus[4], b_bus[2], and_3_5)
        gate.ANDGate2(a_bus[5], b_bus[2], and_2_5)
        gate.ANDGate2(a_bus[6], b_bus[2], and_1_5)
        gate.ANDGate2(a_bus[7], b_bus[2], and_0_5)

        gate.ANDGate2(a_bus[0], b_bus[1], and_7_6)
        gate.ANDGate2(a_bus[1], b_bus[1], and_6_6)
        gate.ANDGate2(a_bus[2], b_bus[1], and_5_6)
        gate.ANDGate2(a_bus[3], b_bus[1], and_4_6)
        gate.ANDGate2(a_bus[4], b_bus[1], and_3_6)
        gate.ANDGate2(a_bus[5], b_bus[1], and_2_6)
        gate.ANDGate2(a_bus[6], b_bus[1], and_1_6)
        gate.ANDGate2(a_bus[7], b_bus[1], and_0_6)

        gate.ANDGate2(a_bus[0], b_bus[0], and_7_7)
        gate.ANDGate2(a_bus[1], b_bus[0], and_6_7)
        gate.ANDGate2(a_bus[2], b_bus[0], and_5_7)
        gate.ANDGate2(a_bus[3], b_bus[0], and_4_7)
        gate.ANDGate2(a_bus[4], b_bus[0], and_3_7)
        gate.ANDGate2(a_bus[5], b_bus[0], and_2_7)
        gate.ANDGate2(a_bus[6], b_bus[0], and_1_7)
        gate.ANDGate2(a_bus[7], b_bus[0], and_0_7)

        bus_1_1 = Bus8(and_7_1, and_6_1, and_5_1, and_4_1, and_3_1, and_2_1,
                       and_1_1, and_0_1)
        bus_1_2 = Bus8(gnd, and_7_0, and_6_0, and_5_0, and_4_0, and_3_0,
                       and_2_0, and_1_0)
        bus_1_out = Bus8(add_1_1, add_1_2, add_1_3, add_1_4, add_1_5, add_1_6,
                         add_1_7, product_bus[14])

        bus_2_1 = Bus8(and_7_2, and_6_2, and_5_2, and_4_2, and_3_2, and_2_2,
                       and_1_2, and_0_2)
        bus_2_2 = Bus8(cout_1, add_1_1, add_1_2, add_1_3, add_1_4, add_1_5,
                       add_1_6, add_1_7)
        bus_2_out = Bus8(add_2_1, add_2_2, add_2_3, add_2_4, add_2_5, add_2_6,
                         add_2_7, product_bus[13])

        bus_3_1 = Bus8(and_7_3, and_6_3, and_5_3, and_4_3, and_3_3, and_2_3,
                       and_1_3, and_0_3)
        bus_3_2 = Bus8(cout_2, add_2_1, add_2_2, add_2_3, add_2_4, add_2_5,
                       add_2_6, add_2_7)
        bus_3_out = Bus8(add_3_1, add_3_2, add_3_3, add_3_4, add_3_5, add_3_6,
                         add_3_7, product_bus[12])

        bus_4_1 = Bus8(and_7_4, and_6_4, and_5_4, and_4_4, and_3_4, and_2_4,
                       and_1_4, and_0_4)
        bus_4_2 = Bus8(cout_3, add_3_1, add_3_2, add_3_3, add_3_4, add_3_5,
                       add_3_6, add_3_7)
        bus_4_out = Bus8(add_4_1, add_4_2, add_4_3, add_4_4, add_4_5, add_4_6,
                         add_4_7, product_bus[11])

        bus_5_1 = Bus8(and_7_5, and_6_5, and_5_5, and_4_5, and_3_5, and_2_5,
                       and_1_5, and_0_5)
        bus_5_2 = Bus8(cout_4, add_4_1, add_4_2, add_4_3, add_4_4, add_4_5,
                       add_4_6, add_4_7)
        bus_5_out = Bus8(add_5_1, add_5_2, add_5_3, add_5_4, add_5_5, add_5_6,
                         add_5_7, product_bus[10])

        bus_6_1 = Bus8(and_7_6, and_6_6, and_5_6, and_4_6, and_3_6, and_2_6,
                       and_1_6, and_0_6)
        bus_6_2 = Bus8(cout_5, add_5_1, add_5_2, add_5_3, add_5_4, add_5_5,
                       add_5_6, add_5_7)
        bus_6_out = Bus8(add_6_1, add_6_2, add_6_3, add_6_4, add_6_5, add_6_6,
                         add_6_7, product_bus[9])

        bus_7_1 = Bus8(and_7_7, and_6_7, and_5_7, and_4_7, and_3_7, and_2_7,
                       and_1_7, and_0_7)
        bus_7_2 = Bus8(cout_6, add_6_1, add_6_2, add_6_3, add_6_4, add_6_5,
                       add_6_6, add_6_7)
        bus_7_out = Bus8(*product_bus[1:9])

        ADD.Adder8(gnd, bus_1_1, bus_1_2, cout_1, bus_1_out)
        ADD.Adder8(gnd, bus_2_1, bus_2_2, cout_2, bus_2_out)
        ADD.Adder8(gnd, bus_3_1, bus_3_2, cout_3, bus_3_out)
        ADD.Adder8(gnd, bus_4_1, bus_4_2, cout_4, bus_4_out)
        ADD.Adder8(gnd, bus_5_1, bus_5_2, cout_5, bus_5_out)
        ADD.Adder8(gnd, bus_6_1, bus_6_2, cout_6, bus_6_out)
        ADD.Adder8(gnd, bus_7_1, bus_7_2, product_bus[0], bus_7_out)
