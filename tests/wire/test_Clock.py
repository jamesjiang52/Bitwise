import bitwise as bw


class TestClock:
    def test_Clock(self):
        clock = bw.wire.Clock()
        assert clock.value == 0

        clock.start()
