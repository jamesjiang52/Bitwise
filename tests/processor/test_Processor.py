import bitwise as bw
import time


class TestProcessor:
    def test_Processor_Sum_Ten_Natural_Numbers(self):
        instruction = bw.processor.Bus10()
        instruction_available = bw.wire.Wire()
        data = bw.wire.Bus8()

        processor_ = bw.processor.Processor(
            instruction,
            instruction_available,
            data
        )

        instruction_available = 1

        # set all registers to 0
        data.wire_values = (0, 0, 0, 0, 0, 0, 0, 0)
        instruction.wire_values = (1, 0, 1, 1, 0, 0, 0, 0, 0, 0)
        instruction.wire_values = (1, 0, 1, 1, 0, 1, 0, 0, 0, 0)
        instruction.wire_values = (1, 0, 1, 1, 1, 0, 0, 0, 0, 0)
        instruction.wire_values = (1, 0, 1, 1, 1, 1, 0, 0, 0, 0)

        # set register 2 to 1
        instruction.wire_values = (1, 0, 1, 0, 0, 0, 0, 0, 1, 0)

        instruction.wire_values = (0, 0, 1, 0, 0, 1, 1, 0, 0, 1)
        instruction.wire_values = (0, 0, 1, 0, 0, 0, 0, 1, 0, 0)
        instruction.wire_values = (0, 0, 1, 0, 0, 1, 1, 0, 0, 1)
        instruction.wire_values = (0, 0, 1, 0, 0, 0, 0, 1, 0, 0)
        instruction.wire_values = (0, 0, 1, 0, 0, 1, 1, 0, 0, 1)
        instruction.wire_values = (0, 0, 1, 0, 0, 0, 0, 1, 0, 0)
        instruction.wire_values = (0, 0, 1, 0, 0, 1, 1, 0, 0, 1)
        instruction.wire_values = (0, 0, 1, 0, 0, 0, 0, 1, 0, 0)
        instruction.wire_values = (0, 0, 1, 0, 0, 1, 1, 0, 0, 1)
        instruction.wire_values = (0, 0, 1, 0, 0, 0, 0, 1, 0, 0)
        instruction.wire_values = (0, 0, 1, 0, 0, 1, 1, 0, 0, 1)
        instruction.wire_values = (0, 0, 1, 0, 0, 0, 0, 1, 0, 0)
        instruction.wire_values = (0, 0, 1, 0, 0, 1, 1, 0, 0, 1)
        instruction.wire_values = (0, 0, 1, 0, 0, 0, 0, 1, 0, 0)
        instruction.wire_values = (0, 0, 1, 0, 0, 1, 1, 0, 0, 1)
        instruction.wire_values = (0, 0, 1, 0, 0, 0, 0, 1, 0, 0)
        instruction.wire_values = (0, 0, 1, 0, 0, 1, 1, 0, 0, 1)
        instruction.wire_values = (0, 0, 1, 0, 0, 0, 0, 1, 0, 0)
        instruction.wire_values = (0, 0, 1, 0, 0, 1, 1, 0, 0, 1)
        instruction.wire_values = (0, 0, 1, 0, 0, 0, 0, 1, 0, 0)

        registers = processor_.get_registers()

        assert registers[0].wire_values == (0, 0, 1, 1, 0, 1, 1, 1)
        assert registers[1].wire_values == (0, 0, 0, 0, 1, 0, 1, 0)
        assert registers[2].wire_values == (0, 0, 0, 0, 0, 0, 0, 1)
        assert registers[3].wire_values == (0, 0, 0, 0, 0, 0, 0, 0)
