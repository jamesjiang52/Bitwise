"""
The following classes are defined:

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
            multiplicant.
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
