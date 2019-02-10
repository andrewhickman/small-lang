func arg => (
  let record = ({
    a: arg,
    b: true,
  }) in (
    if (record) then (true) else (false)
  )
)