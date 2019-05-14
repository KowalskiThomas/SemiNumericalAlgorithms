auto factor3(entier x, entier max_a)-> optional<pair<entier, entier>>
{
	auto a = root<3>(x);
	while (a < max_a)
	{
		auto y = x + power(a, 3);
		auto b = root<3>(y);
		if (power(b, 3) == y)
			return pair((a - b), (a * a + 2 * a * b + b * b);
		a += 1;
	}
	return nullopt;

}
