import bitwise as bw


class TestJKFlipFlop:
    def test_JKFlipFlop(self):
        clock = bw.wire.Wire()
        j = bw.wire.Wire()
        k = bw.wire.Wire()
        output = bw.wire.Wire()
        output_not = bw.wire.Wire()

        a = bw.storage.JKFlipFlop(j, k, clock, output, output_not)

        clock.value = 0
        j.value = 1
        k.value = 0
        clock.value = 1
        assert output.value == 1
        assert output_not.value == 0

        clock.value = 0
        j.value = 0
        k.value = 1
        clock.value = 1
        assert output.value == 0
        assert output_not.value == 1

        clock.value = 0
        j.value = 0
        k.value = 0
        clock.value = 1
        assert output.value == 0
        assert output_not.value == 1

        clock.value = 0
        j.value = 1
        k.value = 1
        clock.value = 1
        assert output.value == 1
        assert output_not.value == 0

        clock.value = 0
        j.value = 0
        k.value = 0
        clock.value = 1
        assert output.value == 1
        assert output_not.value == 0

        print(a.__doc__)
        print(a)

        a(J=1, K=1, clock=0, output=None, output_not=None)
        a(clock=1)
        assert output.value == 0
        assert output_not.value == 1
