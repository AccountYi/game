module Domain.Validation where
  
import ClassyPrelude
import Text.Regex.PCRE.Heavy

-- 验证如果输入是有效的则返回Nothing，否则返回无效。
type Validation e a = a -> Maybe e

--  接受三个参数1.如果验证通过则调用的函数 2.验证函数的列表 3.要验证的数据  
validate :: (a -> b) -> [Validation e a] -> a -> Either [e] b
validate constructor validations val = 
  -- concatMap函数是依次把列表中的值映射到函数中，然后把结果连接起来  concatMap ::（a - > [b]） - > [a] - > [b]
 -- maybeToList函数是 给定Nothing时，maybeToList函数返回一个空列表，如果没有给出Nothing，则返回单个列表。  maybeToList :: Maybe a -> [a]
  case concatMap (\f -> maybeToList $ f val) validations of
    []    -> Right $ constructor val
    errs  -> Left errs

-- 该函数是检查输入值是否在指定范围内。 由于Ord约束，这个函数不仅仅是一个数字，而是适用于所有可以比较的类型
rangeBetween :: (Ord a) => a -> a -> e -> Validation e a
rangeBetween minRange maxRange msg val =
  if val >= minRange && val <= maxRange then Nothing else Just msg

-- 因为我们使用ClassyPrelude中的length函数，所以存在MonoFoldable约束。这基本上意味着此函数适用于所有具有长度的类型，例如Set，List或Map。  
lengthBetween :: (MonoFoldable a) => Int -> Int -> e -> Validation e a
lengthBetween minLen maxLen msg val =
  rangeBetween minLen maxLen msg (length val)
  
--该函数很简单：只需检查该值是否与给定的正则表达式匹配。
regexMatches :: Regex -> e -> Validation e Text
regexMatches regex msg val = 
  if val =~ regex then Nothing else Just msg
