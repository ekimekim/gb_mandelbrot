
from decimal import Decimal
from fractions import Fraction


file = 'math'


class Vecs(object):
	BaseX, BaseY, CX, CY, X, Y, YSq = range(7)


for precision in [2, 3, 8, 255, 256]:
	def encode_value(value):
		"""Returns byte list for vector and sign byte representing given value.
		Value may be string, in which case it's interpreted using Decimal()."""
		if isinstance(value, str):
			value = Decimal(value)
		if value < -4 or value > 4:
			raise ValueError("value out of range")
		if value >= 0:
			sign = 0
		else:
			sign = 1
			value = -value
		value = int(value * 2 ** (8 * precision - 2))
		bytes = []
		for _ in range(precision):
			bytes.append(value % 256)
			value /= 256
		assert value == 0, "value out of range"
		return bytes[::-1], sign

	def random_value():
		"""Gets a uniform random value from -2 to 2"""
		return 4 * Fraction(random.randrange(2 ** (8 * precision))) / 2 ** (8 * precision) - 2

	def vecmem(BaseX=None, BaseY=None, CX=None, CY=None, X=None, Y=None, YSq=None):
		"""Defines what VectorsBase + SignBytes memory range should look like based on values"""
		vecs = []
		signs = []
		for value in (BaseX, BaseY, CX, CY, X, Y, YSq):
			if value is None:
				vec, sign = [], None
			else:
				vec, sign = encode_value(value)
			vec += [None] * (256 - len(vec)) # pad
			vecs += vec
			signs.append(sign)
		return Memory(vecs + signs)

	value = random_value()
	copy = Test('MathCopy',
		in_D=Vecs.X,
		in_H=Vecs.Y,
		in_C=precision - 1,
		in_VectorsBase=vecmem(X=value),
		out_VectorsBase=vecmem(Y=value),
	)

	value = random_value()
	double = Test('MathDouble',
		in_H=Vecs.X,
		in_C=precision - 1,
		in_VectorsBase=vecmem(X=value),
		out_VectorsBase=vecmem(X=2*value),
		out_cflag=0,
	)

	value = 2 + abs(random_value()) # 2 to 4
	double_carry = Test('MathDouble',
		in_H=Vecs.X,
		in_C=precision - 1,
		in_VectorsBase=vecmem(X=value),
		out_VectorsBase=vecmem(X=(2*value) % 4),
		out_cflag=1,
	)

	# find all new tests, rename them to include precision
	for name, value in globals().items():
		if isinstance(value, Test) and not name.endswith("_p"):
			del globals()[name]
			globals()["{}_{}_p".format(name, precision)] = value
