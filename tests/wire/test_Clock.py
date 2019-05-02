import bitwise as bw


class TestClock:
    def test_Clock(self):
        clock = bw.wire.Clock()
        assert clock.value == 0

        # clock.start()

        clock.value = 1
        assert clock.value == 1

        clock.value = 0
        assert clock.value == 0

        print(clock.__doc__)
        print(clock)

        clock(value=1)
        assert clock.value == 1
