import bitwise as bw


class TestRAM256x4:
    def test_RAM256x4(self):
        data_bus = bw.wire.Bus4()
        address_bus = bw.wire.Bus8()
        write_enable = bw.wire.Wire()
        clock = bw.wire.Wire()
        output_bus = bw.wire.Bus4()

        a = bw.storage.RAM256x4(
            data_bus,
            address_bus,
            write_enable,
            clock,
            output_bus
        )

        data_bus.wire_values = (0, 0, 0, 1)
        address_bus.wire_values = (0, 0, 0, 0, 0, 0, 0, 0)
        write_enable.value = 1
        assert output_bus.wire_values == (0, 0, 0, 0)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 0, 1)

        data_bus.wire_values = (1, 0, 1, 0)
        address_bus.wire_values = (1, 0, 0, 0, 0, 0, 0, 0)
        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (1, 0, 1, 0)

        data_bus.wire_values = (1, 1, 1, 0)
        address_bus.wire_values = (1, 1, 1, 1, 1, 1, 1, 1)
        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (1, 1, 1, 0)

        write_enable.value = 0

        address_bus.wire_values = (0, 0, 0, 0, 0, 0, 0, 0)
        assert output_bus.wire_values == (0, 0, 0, 1)

        address_bus.wire_values = (1, 0, 0, 0, 0, 0, 0, 0)
        assert output_bus.wire_values == (1, 0, 1, 0)

        address_bus.wire_values = (1, 1, 1, 1, 1, 1, 1, 1)
        assert output_bus.wire_values == (1, 1, 1, 0)

        print(a.__doc__)
        print(a)

        a(
            data_bus=(0, 0, 0, 0),
            address_bus=(0, 0, 0, 0, 0, 0, 0, 0),
            write_enable=0,
            clock=0,
            output_bus=None
        )
        assert output_bus.wire_values == (0, 0, 0, 1)
