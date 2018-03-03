type value('a) = [> | `List(value('a))] as 'a;

type result('a, 'b) = [< | `Fail(string) | `Success(value('a))] as 'b;

type parser('a, 'b) = string => result('a, 'b);