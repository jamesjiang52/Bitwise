"""
The following classes are defined:
    ShiftRegister4
    ShiftRegister8
    ShiftRegister16
"""

from .. import wire
from .. import gate
from .. import signal
from . import FLOP

Wire = wire.Wire
Bus4 = wire.Bus4
Bus8 = wire.Bus8
Bus16 = wire.Bus16


class ShiftRegister4:
    """Construct a new 4-bit shift register.

    Args:
        enable: An object of type Wire. Enables the shift register.
        clear_n: An object of type Wire. Clears output_bus and output_serial to
            0 asynchronously if its value is 0.
        shift_load: An object of type Wire. The mode select. A value of 0
            indicates a parallel load operation, where output_bus takes on the
            value of data_bus. A value of 1 indicates a shift-right operation,
            where output_bus[3] takes on the value of output_bus[2],
            output_bus[2] takes on the value of output_bus[1], and so on;
            output_bus[0] takes on the value of data_serial.
        data_bus: An object of type Bus4. The parallel data input.
        data_serial. An object of type Wire. The serial data input.
        clock. An object of type Wire or Clock. The clock input to the shift
            register.
        output_bus. An object of type Bus4. The parallel data output.
        output_serial. An object of type Wire. The serial data output.
            Identical to output_bus[3].

    Raises:
        TypeError: If either data_bus or output_bus is not a bus of width 4.
    """
    def __init__(
        self,
        enable,
        reset_n,
        shift_load,
        data_bus,
        data_s,
        clock,
        output_bus,
        output_s
    ):
        if len(data_bus) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(data_bus)
                )
            )

        if len(output_bus) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(output_bus)
                )
            )

        gnd = Wire()
        gnd.value = 0
        vcc = Wire()
        vcc.value = 1

        mux_1_out = Wire()
        mux_2_out = Wire()
        mux_3_out = Wire()
        mux_4_out = Wire()
        q_1_not = Wire()
        q_2_not = Wire()
        q_3_not = Wire()
        q_4_not = Wire()
        and_out = Wire()

        gate.ANDGate2(clock, enable, and_out)
        signal.Multiplexer2To1(
            vcc,
            shift_load,
            data_s,
            data_bus[0],
            mux_1_out
        )
        signal.Multiplexer2To1(
            vcc,
            shift_load,
            output_bus[0],
            data_bus[1],
            mux_2_out
        )
        signal.Multiplexer2To1(
            vcc,
            shift_load,
            output_bus[1],
            data_bus[2],
            mux_3_out
        )
        signal.Multiplexer2To1(
            vcc,
            shift_load,
            output_bus[2],
            data_bus[3],
            mux_4_out
        )

        FLOP.DFlipFlopPresetClear(
            mux_4_out,
            vcc,
            reset_n,
            and_out,
            output_bus[3],
            q_4_not
        )
        FLOP.DFlipFlopPresetClear(
            mux_3_out,
            vcc,
            reset_n,
            and_out,
            output_bus[2],
            q_3_not
        )
        FLOP.DFlipFlopPresetClear(
            mux_2_out,
            vcc,
            reset_n,
            and_out,
            output_bus[1],
            q_2_not
        )
        FLOP.DFlipFlopPresetClear(
            mux_1_out,
            vcc,
            reset_n,
            and_out,
            output_bus[0],
            q_1_not
        )
        gate.Buffer(output_bus[3], output_s)


class ShiftRegister8:
    """Construct a new 8-bit shift register.

    Args:
        enable: An object of type Wire. Enables the shift register.
        clear_n: An object of type Wire. Clears output_bus and output_serial to
            0 asynchronously if its value is 0.
        shift_load: An object of type Wire. The mode select. A value of 0
            indicates a parallel load operation, where output_bus takes on the
            value of data_bus. A value of 1 indicates a shift-right operation,
            where output_bus[7] takes on the value of output_bus[6],
            output_bus[6] takes on the value of output_bus[5], and so on;
            output_bus[0] takes on the value of data_serial.
        data_bus: An object of type Bus8. The parallel data input.
        data_serial. An object of type Wire. The serial data input.
        clock. An object of type Wire or Clock. The clock input to the shift
            register.
        output_bus. An object of type Bus8. The parallel data output.
        output_serial. An object of type Wire. The serial data output.
            Identical to output_bus[7].

    Raises:
        TypeError: If either data_bus or output_bus is not a bus of width 8.
    """
    def __init__(
        self,
        enable,
        reset_n,
        shift_load,
        data_bus,
        data_s,
        clock,
        output_bus,
        output_s
    ):
        if len(data_bus) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(data_bus)
                )
            )

        if len(output_bus) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(output_bus)
                )
            )

        gnd = Wire()
        gnd.value = 0
        vcc = Wire()
        vcc.value = 1

        mux_1_out = Wire()
        mux_2_out = Wire()
        mux_3_out = Wire()
        mux_4_out = Wire()
        mux_5_out = Wire()
        mux_6_out = Wire()
        mux_7_out = Wire()
        mux_8_out = Wire()
        q_1_not = Wire()
        q_2_not = Wire()
        q_3_not = Wire()
        q_4_not = Wire()
        q_5_not = Wire()
        q_6_not = Wire()
        q_7_not = Wire()
        q_8_not = Wire()
        and_out = Wire()

        gate.ANDGate2(enable, clock, and_out)
        signal.Multiplexer2To1(
            vcc,
            shift_load,
            data_s,
            data_bus[0],
            mux_1_out
        )
        signal.Multiplexer2To1(
            vcc,
            shift_load,
            output_bus[0],
            data_bus[1],
            mux_2_out
        )
        signal.Multiplexer2To1(
            vcc,
            shift_load,
            output_bus[1],
            data_bus[2],
            mux_3_out
        )
        signal.Multiplexer2To1(
            vcc,
            shift_load,
            output_bus[2],
            data_bus[3],
            mux_4_out
        )
        signal.Multiplexer2To1(
            vcc,
            shift_load,
            output_bus[3],
            data_bus[4],
            mux_5_out
        )
        signal.Multiplexer2To1(
            vcc,
            shift_load,
            output_bus[4],
            data_bus[5],
            mux_6_out
        )
        signal.Multiplexer2To1(
            vcc,
            shift_load,
            output_bus[5],
            data_bus[6],
            mux_7_out
        )
        signal.Multiplexer2To1(
            vcc,
            shift_load,
            output_bus[6],
            data_bus[7],
            mux_8_out
        )
        FLOP.DFlipFlopPresetClear(
            mux_8_out,
            vcc,
            reset_n,
            and_out,
            output_bus[7],
            q_8_not
        )
        FLOP.DFlipFlopPresetClear(
            mux_7_out,
            vcc,
            reset_n,
            and_out,
            output_bus[6],
            q_7_not
        )
        FLOP.DFlipFlopPresetClear(
            mux_6_out,
            vcc,
            reset_n,
            and_out,
            output_bus[5],
            q_6_not
        )
        FLOP.DFlipFlopPresetClear(
            mux_5_out,
            vcc,
            reset_n,
            and_out,
            output_bus[4],
            q_5_not
        )
        FLOP.DFlipFlopPresetClear(
            mux_4_out,
            vcc,
            reset_n,
            and_out,
            output_bus[3],
            q_4_not
        )
        FLOP.DFlipFlopPresetClear(
            mux_3_out,
            vcc,
            reset_n,
            and_out,
            output_bus[2],
            q_3_not
        )
        FLOP.DFlipFlopPresetClear(
            mux_2_out,
            vcc,
            reset_n,
            and_out,
            output_bus[1],
            q_2_not
        )
        FLOP.DFlipFlopPresetClear(
            mux_1_out,
            vcc,
            reset_n,
            and_out,
            output_bus[0],
            q_1_not
        )
        gate.Buffer(output_bus[7], output_s)


class ShiftRegister16:
    """Construct a new 16-bit shift register.

    Args:
        enable: An object of type Wire. Enables the shift register.
        clear_n: An object of type Wire. Clears output_bus and output_serial to
            0 asynchronously if its value is 0.
        shift_load: An object of type Wire. The mode select. A value of 0
            indicates a parallel load operation, where output_bus takes on the
            value of data_bus. A value of 1 indicates a shift-right operation,
            where output_bus[15] takes on the value of output_bus[14],
            output_bus[14] takes on the value of output_bus[13], and so on;
            output_bus[0] takes on the value of data_serial.
        data_bus: An object of type Bus16. The parallel data input.
        data_serial. An object of type Wire. The serial data input.
        clock. An object of type Wire or Clock. The clock input to the shift
            register.
        output_bus. An object of type Bus16. The parallel data output.
        output_serial. An object of type Wire. The serial data output.
            Identical to output_bus[15].

    Raises:
        TypeError: If either data_bus or output_bus is not a bus of width 16.
    """
    def __init__(
        self,
        enable,
        reset_n,
        shift_load,
        data_bus,
        data_s,
        clock,
        output_bus,
        output_s
    ):
        if len(data_bus) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(data_bus)
                )
            )

        if len(output_bus) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(output_bus)
                )
            )

        gnd = Wire()
        gnd.value = 0
        vcc = Wire()
        vcc.value = 1

        mux_1_out = Wire()
        mux_2_out = Wire()
        mux_3_out = Wire()
        mux_4_out = Wire()
        mux_5_out = Wire()
        mux_6_out = Wire()
        mux_7_out = Wire()
        mux_8_out = Wire()
        mux_9_out = Wire()
        mux_10_out = Wire()
        mux_11_out = Wire()
        mux_12_out = Wire()
        mux_13_out = Wire()
        mux_14_out = Wire()
        mux_15_out = Wire()
        mux_16_out = Wire()
        q_1_not = Wire()
        q_2_not = Wire()
        q_3_not = Wire()
        q_4_not = Wire()
        q_5_not = Wire()
        q_6_not = Wire()
        q_7_not = Wire()
        q_8_not = Wire()
        q_9_not = Wire()
        q_10_not = Wire()
        q_11_not = Wire()
        q_12_not = Wire()
        q_13_not = Wire()
        q_14_not = Wire()
        q_15_not = Wire()
        q_16_not = Wire()
        and_out = Wire()

        gate.ANDGate2(enable, clock, and_out)
        signal.Multiplexer2To1(
            vcc,
            shift_load,
            data_s,
            data_bus[0],
            mux_1_out
        )
        signal.Multiplexer2To1(
            vcc,
            shift_load,
            output_bus[0],
            data_bus[1],
            mux_2_out
        )
        signal.Multiplexer2To1(
            vcc,
            shift_load,
            output_bus[1],
            data_bus[2],
            mux_3_out
        )
        signal.Multiplexer2To1(
            vcc,
            shift_load,
            output_bus[2],
            data_bus[3],
            mux_4_out
        )
        signal.Multiplexer2To1(
            vcc,
            shift_load,
            output_bus[3],
            data_bus[4],
            mux_5_out
        )
        signal.Multiplexer2To1(
            vcc,
            shift_load,
            output_bus[4],
            data_bus[5],
            mux_6_out
        )
        signal.Multiplexer2To1(
            vcc,
            shift_load,
            output_bus[5],
            data_bus[6],
            mux_7_out
        )
        signal.Multiplexer2To1(
            vcc,
            shift_load,
            output_bus[6],
            data_bus[7],
            mux_8_out
        )
        signal.Multiplexer2To1(
            vcc,
            shift_load,
            output_bus[7],
            data_bus[8],
            mux_9_out
        )
        signal.Multiplexer2To1(
            vcc,
            shift_load,
            output_bus[8],
            data_bus[9],
            mux_10_out
        )
        signal.Multiplexer2To1(
            vcc,
            shift_load,
            output_bus[9],
            data_bus[10],
            mux_11_out
        )
        signal.Multiplexer2To1(
            vcc,
            shift_load,
            output_bus[10],
            data_bus[11],
            mux_12_out
        )
        signal.Multiplexer2To1(
            vcc,
            shift_load,
            output_bus[11],
            data_bus[12],
            mux_13_out
        )
        signal.Multiplexer2To1(
            vcc,
            shift_load,
            output_bus[12],
            data_bus[13],
            mux_14_out
        )
        signal.Multiplexer2To1(
            vcc,
            shift_load,
            output_bus[13],
            data_bus[14],
            mux_15_out
        )
        signal.Multiplexer2To1(
            vcc,
            shift_load,
            output_bus[14],
            data_bus[15],
            mux_16_out
        )
        FLOP.DFlipFlopPresetClear(
            mux_16_out,
            vcc,
            reset_n,
            and_out,
            output_bus[15],
            q_16_not
        )
        FLOP.DFlipFlopPresetClear(
            mux_15_out,
            vcc,
            reset_n,
            and_out,
            output_bus[14],
            q_15_not
        )
        FLOP.DFlipFlopPresetClear(
            mux_14_out,
            vcc,
            reset_n,
            and_out,
            output_bus[13],
            q_14_not
        )
        FLOP.DFlipFlopPresetClear(
            mux_13_out,
            vcc,
            reset_n,
            and_out,
            output_bus[12],
            q_13_not
        )
        FLOP.DFlipFlopPresetClear(
            mux_12_out,
            vcc,
            reset_n,
            and_out,
            output_bus[11],
            q_12_not
        )
        FLOP.DFlipFlopPresetClear(
            mux_11_out,
            vcc,
            reset_n,
            and_out,
            output_bus[10],
            q_11_not
        )
        FLOP.DFlipFlopPresetClear(
            mux_10_out,
            vcc,
            reset_n,
            and_out,
            output_bus[9],
            q_10_not
        )
        FLOP.DFlipFlopPresetClear(
            mux_9_out,
            vcc,
            reset_n,
            and_out,
            output_bus[8],
            q_9_not
        )
        FLOP.DFlipFlopPresetClear(
            mux_8_out,
            vcc,
            reset_n,
            and_out,
            output_bus[7],
            q_8_not
        )
        FLOP.DFlipFlopPresetClear(
            mux_7_out,
            vcc,
            reset_n,
            and_out,
            output_bus[6],
            q_7_not
        )
        FLOP.DFlipFlopPresetClear(
            mux_6_out,
            vcc,
            reset_n,
            and_out,
            output_bus[5],
            q_6_not
        )
        FLOP.DFlipFlopPresetClear(
            mux_5_out,
            vcc,
            reset_n,
            and_out,
            output_bus[4],
            q_5_not
        )
        FLOP.DFlipFlopPresetClear(
            mux_4_out,
            vcc,
            reset_n,
            and_out,
            output_bus[3],
            q_4_not
        )
        FLOP.DFlipFlopPresetClear(
            mux_3_out,
            vcc,
            reset_n,
            and_out,
            output_bus[2],
            q_3_not
        )
        FLOP.DFlipFlopPresetClear(
            mux_2_out,
            vcc,
            reset_n,
            and_out,
            output_bus[1],
            q_2_not
        )
        FLOP.DFlipFlopPresetClear(
            mux_1_out,
            vcc,
            reset_n,
            and_out,
            output_bus[0],
            q_1_not
        )
        gate.Buffer(output_bus[15], output_s)
