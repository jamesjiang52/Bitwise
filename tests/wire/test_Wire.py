import bitwise as bw


class TestWire:
    def test_Wire(self):
        wire = bw.wire.Wire()
        assert wire.value == 0

        wire.value = 1
        assert wire.value == 1

        wire.value = 0
        assert wire.value == 0
