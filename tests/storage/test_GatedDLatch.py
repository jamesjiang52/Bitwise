import bitwise as bw


class TestGatedDLatch:
    def test_GatedDLatch(self):
        clock = bw.wire.Wire()
        data = bw.wire.Wire()
        output = bw.wire.Wire()
        output_not = bw.wire.Wire()

        a = bw.storage.GatedDLatch(data, clock, output, output_not)

        clock.value = 1
        data.value = 0
        assert output.value == 0
        assert output_not.value == 1

        clock.value = 1
        data.value = 1
        assert output.value == 1
        assert output_not.value == 0

        clock.value = 0
        data.value = 1
        assert output.value == 1
        assert output_not.value == 0

        clock.value = 0
        data.value = 0
        assert output.value == 1
        assert output_not.value == 0

        clock.value = 1
        data.value = 0
        assert output.value == 0
        assert output_not.value == 1

        clock.value = 0
        data.value = 0
        assert output.value == 0
        assert output_not.value == 1

        clock.value = 0
        data.value = 1
        assert output.value == 0
        assert output_not.value == 1

        clock.value = 1
        data.value = 1
        assert output.value == 1
        assert output_not.value == 0

        print(a.__doc__)
        print(a)

        a(data=0, clock=1, output=None, output_not=None)
        assert output.value == 0
        assert output_not.value == 1
