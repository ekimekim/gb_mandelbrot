
from decimal import Decimal
from fractions import Fraction


file = 'math'


class Vecs(object):
	BaseX, BaseY, CX, CY, X, Y, YSq = range(7)


def model_mul(v1, v2, precision):
	"""Takes two lists of bytes that are encoded values PRECISION bytes long,
	and multiplies them according to the same algorithm used in the asm."""
	def partial_add(a, b, p):
		carry = 0
		for i in range(p)[::-1]:
			carry, a[i] = divmod(a[i] + b[i] + carry, 256)
		return carry

	def shift_right(a):
		carry = 0
		for i in range(len(a)):
			a[i], carry = divmod(a[i] + 256 * carry, 2)
		return carry

	# makes a copy of v2 with an extra byte prepended,
	# in real code this is in-place.
	result = [0] + v2

	for i in range(precision):
		for j in range(8):
			next_bit = shift_right(result)
			if next_bit:
				carry = partial_add(result, v1, i + 1)
				if carry:
					raise ValueError("overflow")

	for x in range(7):
		shift_right(result)

	if result[0]:
		raise ValueError("overflow")

	return result[1:]


for precision in [2, 3, 8, 255, 256]:
	def encode_value(value):
		"""Returns byte list for vector and sign byte representing given value.
		Value may be string, in which case it's interpreted using Decimal()."""
		if isinstance(value, str):
			value = Decimal(value)
		if value < -4 or value > 4:
			raise ValueError("value out of range: {}".format(value))
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
		assert value == 0, "value out of range, got remainder {}".format(value)
		return bytes[::-1], sign

	def uniform(exp=0):
		"""Gets a uniform random value in [0, 2^exp) which does not exceed current precision"""
		return Fraction(random.randrange(2 ** (8 * precision - 2 + exp))) / 2 ** (8 * precision - 2)

	def random_value():
		"""Gets a uniform random value from -2 to 2 (both exclusive)"""
		sign = random.choice([-1, 1])
		value = uniform(exp=1)
		return sign * value

	def vecmem(BaseX=None, BaseY=None, CX=None, CY=None, X=None, Y=None, YSq=None):
		"""Defines what VectorsBase + SignBytes memory range should look like based on values"""
		vecs = []
		signs = []
		for value in (BaseX, BaseY, CX, CY, X, Y, YSq):
			if value is None:
				vec, sign = [], None
			elif isinstance(value, tuple):
				vec, sign = value
			else:
				vec, sign = encode_value(value)
			vec += [None] * (256 - len(vec)) # pad
			vecs += vec
			signs.append(sign)
		return Memory(vecs + signs)

	def test_regs(in1=None, in2=None, out=None, carry=None):
		kwargs = {'in_C': precision - 1, 'in_H': Vecs.X}
		in_vecs = {}
		if in1 is not None:
			in_vecs['X'] = in1
		if in2 is not None:
			kwargs['in_D'] = Vecs.Y
			in_vecs['Y'] = in2
		if in_vecs:
			kwargs['in_VectorsBase'] = vecmem(**in_vecs)
		if out is not None:
			kwargs['out_VectorsBase'] = vecmem(X=out)
		if carry is not None:
			kwargs['out_cflag'] = carry
		return kwargs

	value = random_value()
	copy = Test('MathCopy', **test_regs(in2=value, out=value))

	value = random_value()
	double = Test('MathDouble', **test_regs(in1=value, out=2*value, carry=0))

	value = 2 + uniform(exp=1) # 2 to 4
	double_carry = Test('MathDouble', **test_regs(in1=value, out=(2*value) % 4, carry=1))

	value1 = uniform() # 0 to 1
	value2 = 1 + uniform() # 1 to 2
	add_pos_pos = Test('MathAdd', **test_regs(in1=value1, in2=value2, out=value1+value2, carry=0))
	add_sub_sub = Test('MathAdd', **test_regs(in1=-value1, in2=-value2, out=-(value1+value2), carry=0))
	add_pos_sub_no_underflow = Test('MathAdd', **test_regs(in1=-value1, in2=value2, out=value2-value1, carry=0))
	add_pos_sub_underflow = Test('MathAdd', **test_regs(in1=value1, in2=-value2, out=value1-value2, carry=0))
	add_carry = Test('MathAdd', **test_regs(in1=3 + value1, in2=value2, out=(3+value1+value2) % 4, carry=1))
	sub_pos_pos = Test('MathSub', **test_regs(in1=value1, in2=value2, out=value1-value2, carry=0))

	value1 = uniform(exp=1)
	value2 = uniform(exp=1)
	v1e, v2e = [encode_value(v)[0] for v in (value1, value2)]
	square_result = model_mul(v1e, v1e, precision)
	mul_result = model_mul(v1e, v2e, precision)
	square = Test('MathSquare', **test_regs(in2=value1, out=(square_result, 0), carry=0))
	mul_pos_pos = Test('MathMultiply', **test_regs(in1=value1, in2=value2, out=(mul_result, 0), carry=0))
	mul_pos_neg = Test('MathMultiply', **test_regs(in1=value1, in2=-value2, out=(mul_result, 1), carry=0))
	square_overflow = Test('MathSquare', **test_regs(in2=2 + abs(value1), carry=1))

	full_escape = Test('GetIterations', in_C=precision-1, in_B=32,
		in_VectorsBase = vecmem(CX='-0.8', CY='-0.2'),
		out_B=16,
	)
	full_no_escape = Test('GetIterations', in_C=precision-1, in_B=250,
		in_VectorsBase = vecmem(CX='-0.1', CY='0.7'),
		out_B=0,
	)

	# find all new tests, rename them to include precision
	for name, value in globals().items():
		if isinstance(value, Test) and not name.endswith("_p"):
			del globals()[name]
			globals()["{}_{}_p".format(name, precision)] = value
