import bitwise as bw


class TestProcessorControlpath:
    def __init__(self):
        clock = bw.wire.Wire()
        instruction = bw.wire.Bus16()
        z = bw.wire.Wire()
        v = bw.wire.Wire()
        n = bw.wire.Wire()
        c = bw.wire.Wire()
        reg_in = bw.wire.Bus16()
        reg_out = bw.wire.Bus16()
        up_sp = bw.wire.Wire()
        down_sp = bw.wire.Wire()
        up_pc = bw.wire.Wire()
        load_pc = bw.wire.Wire()
        a_in = bw.wire.Wire()
        flags_in = bw.wire.Wire()
        g_in = bw.wire.Wire()
        g_out = bw.wire.Wire()
        data_in = bw.wire.Wire()
        addr_in = bw.wire.Wire()
        sel_bus = bw.wire.Bus4()
        ext = bw.wire.Wire()
        ir_in = bw.wire.Wire()
        write_enable = bw.wire.Wire()

        control = bw.processor.ProcessorControlpath(
            clock,
            instruction,
            z,
            v,
            n,
            c,
            reg_in,
            reg_out,
            up_sp,
            down_sp,
            up_pc,
            load_pc,
            a_in,
            flags_in,
            g_in,
            g_out,
            data_in,
            addr_in,
            sel_bus,
            ext,
            ir_in,
            write_enable
        )

        instruction.wire_values = (0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0)
        clock.value = 0
        clock.value = 1
        control.print_wire_values()

        instruction.wire_values = (0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0)
        clock.value = 0
        clock.value = 1
        control.print_wire_values()

        clock.value = 0
        clock.value = 1
        control.print_wire_values()

        clock.value = 0
        clock.value = 1
        control.print_wire_values()

        clock.value = 0
        clock.value = 1
        control.print_wire_values()

        clock.value = 0
        clock.value = 1
        control.print_wire_values()

        clock.value = 0
        clock.value = 1
        control.print_wire_values()

        clock.value = 0
        clock.value = 1
        control.print_wire_values()

        clock.value = 0
        clock.value = 1
        control.print_wire_values()

        clock.value = 0
        clock.value = 1
        control.print_wire_values()
