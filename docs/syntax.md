# 语法规范

类型：

```
T :=
	Int | Bool | T -> T | (T)
	() | (T,) | (T, ..., T) |	// 积类型或元组类型，可以为空，如果只有一个T则必须在末尾加一个逗号
	(T | ... | T) |				// 和类型或union类型，至少要有一个
	[T]							// 列表类型
```

表达式：

```
E :=
	int | bool | (E)
	op E | E op E |						// 一元运算 + - !；二元运算 + - * / && ||
	if E then E else E |

	() | (E,) | (E, ..., E)				// 元组构造
	E.int								// 元组解开，下标必须是整数字面值
	union (T | ... | T E | ... | T)		// union构造，要恰好其中一项有值
	case E of (id:T E | ... | id:T E)	// union解开
	[E, ..., E]							// 列表构造
	E[E]								// 列表索引

	lambda id:T to E |					// lambda表达式
	E E									// 函数调用
	fix E
```

语句：

```
S :=
	let id:T = E
```