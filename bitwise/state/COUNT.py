"""
The following classes are defined:
    UpCounterMod4
    UpCounterMod8
    UpCounterMod16
    DownCounterMod4
    DownCounterMod8
    DownCounterMod16
"""

from .. import wire
from .. import gate
from .. import signal
from .. import storage

Wire = wire.Wire
Bus4 = wire.Bus4
Bus8 = wire.Bus8
Bus16 = wire.Bus16


class UpCounterMod4:
    """Construct a new mod-4 up counter.

    Args:
        enable: An object of type Wire. Enables the counter.
        clear_n: An object of type Wire. Clears output_1 and output_2 to 0
            asynchronously if its value is 0.
        clock: An object of type Wire or Clock. The clock input to the counter.
        output_1: An object of type Wire. The most significant bit of the
            output.
        output_2: An object of type Wire. The least significant bit of the
            output.
    """
    def __init__(self, enable, clear_n, clock, output_1, output_2):
        vcc = Wire()
        vcc.value = 1

        output_not_1 = Wire()
        output_not_2 = Wire()
        and_1 = Wire()

        gate.ANDGate2(enable, output_2, and_1)

        storage.TFlipFlopPresetClear(
            and_1,
            vcc,
            clear_n,
            clock,
            output_1,
            output_not_1
        )
        storage.TFlipFlopPresetClear(
            enable,
            vcc,
            clear_n,
            clock,
            output_2,
            output_not_2
        )

        self.enable = enable
        self.clear_n = clear_n
        self.clock = clock
        self.output_1 = output_1
        self.output_2 = output_2

    def __str__(self):
        str_ = ""
        str_ += "enable: " + str(self.enable.value) + "\n"
        str_ += "clear_n: " + str(self.clear_n.value) + "\n"
        str_ += "clock: " + str(self.clock.value) + "\n"
        str_ += "output_1: " + str(self.output_1.value) + "\n"
        str_ += "output_2: " + str(self.output_2.value)
        return str_


class UpCounterMod8:
    """Construct a new mod-8 up counter.

    Args:
        enable: An object of type Wire. Enables the counter.
        clear_n: An object of type Wire. Clears output_1, output_2, and
            output_3 to 0 asynchronously if its value is 0.
        clock: An object of type Wire or Clock. The clock input to the counter.
        output_1: An object of type Wire. The most significant bit of the
            output.
        output_2: An object of type Wire.
        output_3: An object of type Wire. The least significant bit of the
            output.
    """
    def __init__(self, enable, clear_n, clock, output_1, output_2, output_3):
        vcc = Wire()
        vcc.value = 1

        output_not_1 = Wire()
        output_not_2 = Wire()
        output_not_3 = Wire()
        and_1 = Wire()
        and_2 = Wire()

        gate.ANDGate2(enable, output_3, and_1)
        gate.ANDGate2(and_1, output_2, and_2)

        storage.TFlipFlopPresetClear(
            and_2,
            vcc,
            clear_n,
            clock,
            output_1,
            output_not_1
        )
        storage.TFlipFlopPresetClear(
            and_1,
            vcc,
            clear_n,
            clock,
            output_2,
            output_not_2
        )
        storage.TFlipFlopPresetClear(
            enable,
            vcc,
            clear_n,
            clock,
            output_3,
            output_not_3
        )

        self.enable = enable
        self.clear_n = clear_n
        self.clock = clock
        self.output_1 = output_1
        self.output_2 = output_2
        self.output_3 = output_3

    def __str__(self):
        str_ = ""
        str_ += "enable: " + str(self.enable.value) + "\n"
        str_ += "clear_n: " + str(self.clear_n.value) + "\n"
        str_ += "clock: " + str(self.clock.value) + "\n"
        str_ += "output_1: " + str(self.output_1.value) + "\n"
        str_ += "output_2: " + str(self.output_2.value) + "\n"
        str_ += "output_3: " + str(self.output_3.value)
        return str_


class UpCounterMod16:
    """Construct a new mod-16 up counter.

    Args:
        enable: An object of type Wire. Enables the counter.
        clear_n: An object of type Wire. Clears output_bus to 0 asynchronously
            if its value is 0.
        clock: An object of type Wire or Clock. The clock input to the counter.
        output_bus: An object of type Bus4. The output of the counter.
            output_bus[0] and output_bus[3] are the most and least significant
            bit, respectively.

    Raises:
        TypeError: If output_bus is not a bus of width 4.
    """
    def __init__(self, enable, clear_n, clock, output_bus):
        if len(output_bus) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(output_bus)
                )
            )

        vcc = Wire()
        vcc.value = 1

        output_not_1 = Wire()
        output_not_2 = Wire()
        output_not_3 = Wire()
        output_not_4 = Wire()
        and_1 = Wire()
        and_2 = Wire()
        and_3 = Wire()

        gate.ANDGate2(enable, output_bus[3], and_1)
        gate.ANDGate2(and_1, output_bus[2], and_2)
        gate.ANDGate2(and_2, output_bus[1], and_3)

        storage.TFlipFlopPresetClear(
            and_3,
            vcc,
            clear_n,
            clock,
            output_bus[0],
            output_not_1
        )
        storage.TFlipFlopPresetClear(
            and_2,
            vcc,
            clear_n,
            clock,
            output_bus[1],
            output_not_2
        )
        storage.TFlipFlopPresetClear(
            and_1,
            vcc,
            clear_n,
            clock,
            output_bus[2],
            output_not_3
        )
        storage.TFlipFlopPresetClear(
            enable,
            vcc,
            clear_n,
            clock,
            output_bus[3],
            output_not_4
        )

        self.enable = enable
        self.clear_n = clear_n
        self.clock = clock
        self.output_bus = output_bus

    def __str__(self):
        str_ = ""
        str_ += "enable: " + str(self.enable.value) + "\n"
        str_ += "clear_n: " + str(self.clear_n.value) + "\n"
        str_ += "clock: " + str(self.clock.value) + "\n"
        str_ += "output_bus: " + self.output_bus.__str__()
        return str_


class DownCounterMod4:
    """Construct a new mod-4 down counter.

    Args:
        enable: An object of type Wire. Enables the counter.
        load_n: An object of type Wire. Loads load_1 into output_1 and load_2
            into output_2 if its value is 0.
        load_1: An object of type Wire. The most significant bit of the load
            input.
        load_2: An object of type Wire. The least significant bit of the load
            input.
        clock: An object of type Wire or Clock. The clock input to the counter.
        output_1: An object of type Wire. The most significant bit of the
            output.
        output_2: An object of type Wire. The least significant bit of the
            output.
    """
    def __init__(
        self,
        enable,
        load_n,
        load_1,
        load_2,
        clock,
        output_1,
        output_2
    ):
        vcc = Wire()
        vcc.value = 1

        mux_1 = Wire()
        mux_2 = Wire()
        output_not_1 = Wire()
        output_not_2 = Wire()
        and_1 = Wire()
        not_1 = Wire()
        xor_1 = Wire()
        xor_2 = Wire()

        gate.NOTGate(output_2, not_1)
        gate.ANDGate2(enable, not_1, and_1)
        gate.XORGate2(enable, output_2, xor_1)
        gate.XORGate2(and_1, output_1, xor_2)

        signal.Multiplexer2To1(vcc, load_n, xor_1, load_2, mux_1)
        signal.Multiplexer2To1(vcc, load_n, xor_2, load_1, mux_2)
        storage.DFlipFlop(
            mux_2,
            clock,
            output_1,
            output_not_1
        )
        storage.DFlipFlop(
            mux_1,
            clock,
            output_2,
            output_not_2
        )

        self.enable = enable
        self.load_n = load_n
        self.load_1 = load_1
        self.load_2 = load_2
        self.clock = clock
        self.output_1 = output_1
        self.output_2 = output_2

    def __str__(self):
        str_ = ""
        str_ += "enable: " + str(self.enable.value) + "\n"
        str_ += "load_n: " + str(self.load_n.value) + "\n"
        str_ += "load_1: " + str(self.load_1.value) + "\n"
        str_ += "load_2: " + str(self.load_2.value) + "\n"
        str_ += "clock: " + str(self.clock.value) + "\n"
        str_ += "output_1: " + str(self.output_1.value) + "\n"
        str_ += "output_2: " + str(self.output_2.value)
        return str_


class DownCounterMod8:
    """Construct a new mod-8 down counter.

    Args:
        enable: An object of type Wire. Enables the counter.
        load_n: An object of type Wire. Loads load_1 into output_1, load_2 into
            output_2, and load_3 into output_3 if its value is 0.
        load_1: An object of type Wire. The most significant bit of the load
            input.
        load_2: An object of type Wire.
        load_3: An object of type Wire. The least significant bit of the load
            input.
        clock: An object of type Wire or Clock. The clock input to the counter.
        output_1: An object of type Wire. The most significant bit of the
            output.
        output_2: An object of type Wire.
        output_3: An object of type Wire. The least significant bit of the
            output.
    """
    def __init__(
        self,
        enable,
        load_n,
        load_1,
        load_2,
        load_3,
        clock,
        output_1,
        output_2,
        output_3
    ):
        vcc = Wire()
        vcc.value = 1

        mux_1 = Wire()
        mux_2 = Wire()
        mux_3 = Wire()
        output_not_1 = Wire()
        output_not_2 = Wire()
        output_not_3 = Wire()
        and_1 = Wire()
        and_2 = Wire()
        not_1 = Wire()
        not_2 = Wire()
        xor_1 = Wire()
        xor_2 = Wire()
        xor_3 = Wire()

        gate.NOTGate(output_3, not_1)
        gate.NOTGate(output_2, not_2)
        gate.ANDGate2(enable, not_1, and_1)
        gate.ANDGate2(and_1, not_2, and_2)
        gate.XORGate2(enable, output_3, xor_1)
        gate.XORGate2(and_1, output_2, xor_2)
        gate.XORGate2(and_2, output_1, xor_3)

        signal.Multiplexer2To1(vcc, load_n, xor_1, load_3, mux_1)
        signal.Multiplexer2To1(vcc, load_n, xor_2, load_2, mux_2)
        signal.Multiplexer2To1(vcc, load_n, xor_3, load_1, mux_3)
        storage.DFlipFlop(
            mux_3,
            clock,
            output_1,
            output_not_1
        )
        storage.DFlipFlop(
            mux_2,
            clock,
            output_2,
            output_not_2
        )
        storage.DFlipFlop(
            mux_1,
            clock,
            output_3,
            output_not_3
        )

        self.enable = enable
        self.load_n = load_n
        self.load_1 = load_1
        self.load_2 = load_2
        self.load_3 = load_3
        self.clock = clock
        self.output_1 = output_1
        self.output_2 = output_2
        self.output_3 = output_3

    def __str__(self):
        str_ = ""
        str_ += "enable: " + str(self.enable.value) + "\n"
        str_ += "load_n: " + str(self.load_n.value) + "\n"
        str_ += "load_1: " + str(self.load_1.value) + "\n"
        str_ += "load_2: " + str(self.load_2.value) + "\n"
        str_ += "load_3: " + str(self.load_3.value) + "\n"
        str_ += "clock: " + str(self.clock.value) + "\n"
        str_ += "output_1: " + str(self.output_1.value) + "\n"
        str_ += "output_2: " + str(self.output_2.value) + "\n"
        str_ += "output_3: " + str(self.output_3.value)
        return str_


class DownCounterMod16:
    """Construct a new mod-16 down counter.

    Args:
        enable: An object of type Wire. Enables the counter.
        load_n: An object of type Wire. Loads load_bus into output_bus if its
            value is 0.
        load_bus: An object of type Bus4. The load input to the counter.
            load_bus[0] and load_bus[3] are the most and least significant bit,
            respectively.
        clock: An object of type Wire or Clock. The clock input to the counter.
        output_bus: An object of type Bus4. The output of the counter.
            output_bus[0] and output_bus[3] are the most and least significant
            bit, respectively.

    Raises:
        TypeError: If either load_bus or output_bus is not a bus of width 4.
    """
    def __init__(
        self,
        enable,
        load_n,
        load_bus,
        clock,
        output_bus
    ):
        if len(load_bus) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(load_bus)
                )
            )

        if len(output_bus) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(output_bus)
                )
            )

        vcc = Wire()
        vcc.value = 1

        mux_1 = Wire()
        mux_2 = Wire()
        mux_3 = Wire()
        mux_4 = Wire()
        output_not_1 = Wire()
        output_not_2 = Wire()
        output_not_3 = Wire()
        output_not_4 = Wire()
        and_1 = Wire()
        and_2 = Wire()
        and_3 = Wire()
        not_1 = Wire()
        not_2 = Wire()
        not_3 = Wire()
        xor_1 = Wire()
        xor_2 = Wire()
        xor_3 = Wire()
        xor_4 = Wire()

        gate.NOTGate(output_bus[3], not_1)
        gate.NOTGate(output_bus[2], not_2)
        gate.NOTGate(output_bus[1], not_3)
        gate.ANDGate2(enable, not_1, and_1)
        gate.ANDGate2(and_1, not_2, and_2)
        gate.ANDGate2(and_2, not_3, and_3)
        gate.XORGate2(enable, output_bus[3], xor_1)
        gate.XORGate2(and_1, output_bus[2], xor_2)
        gate.XORGate2(and_2, output_bus[1], xor_3)
        gate.XORGate2(and_3, output_bus[0], xor_4)

        signal.Multiplexer2To1(vcc, load_n, xor_1, load_bus[3], mux_1)
        signal.Multiplexer2To1(vcc, load_n, xor_2, load_bus[2], mux_2)
        signal.Multiplexer2To1(vcc, load_n, xor_3, load_bus[1], mux_3)
        signal.Multiplexer2To1(vcc, load_n, xor_4, load_bus[0], mux_4)
        storage.DFlipFlop(
            mux_4,
            clock,
            output_bus[0],
            output_not_1
        )
        storage.DFlipFlop(
            mux_3,
            clock,
            output_bus[1],
            output_not_2
        )
        storage.DFlipFlop(
            mux_2,
            clock,
            output_bus[2],
            output_not_3
        )
        storage.DFlipFlop(
            mux_1,
            clock,
            output_bus[3],
            output_not_4
        )

        self.enable = enable
        self.load_n = load_n
        self.load_bus = load_bus
        self.clock = clock
        self.output_bus = output_bus

    def __str__(self):
        str_ = ""
        str_ += "enable: " + str(self.enable.value) + "\n"
        str_ += "load_n: " + str(self.load_n.value) + "\n"
        str_ += "load_bus: " + self.load_bus.__str__() + "\n"
        str_ += "clock: " + str(self.clock.value) + "\n"
        str_ += "output_bus: " + self.output_bus.__str__()
        return str_
