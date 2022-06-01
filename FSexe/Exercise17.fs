// 43.3 Реализуйте стандартную функцию Map.tryFind самостоятельно в виде функции try_find.
let try_find key table =
      try
      Some (Map.find key table)
      with
      | :? System.Collections.Generic.KeyNotFoundException -> None

