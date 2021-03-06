import bitwise as bw


class TestTristateBuffer:
    def test_TristateBuffer(self):
        input_1 = bw.wire.Wire()
        switch = bw.wire.Wire()
        output = bw.wire.Wire()

        a = bw.wire.TristateBuffer(switch, input_1, output)

        switch.value = 0
        input_1.value = 0
        assert output.value == 0

        input_1.value = 1
        assert output.value == 0

        switch.value = 1
        assert output.value == 1

        input_1.value = 0
        assert output.value == 0

        input_1.value = 1
        assert output.value == 1

        switch.value = 0
        assert output.value == 1

        input_1.value = 0
        assert output.value == 1

        input_1.value = 1
        output.value = 0
        assert input_1.value == 1

        print(a.__doc__)
        print(a)

        a(enable=1, input=0, output=None)
        assert output.value == 0
