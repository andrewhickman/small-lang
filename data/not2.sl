func arg => (
  let not = (
    func x => (
      if (x) then (
        true
      ) else (
        false
      )
    )
  ) in (
    (not) ((not) (arg))
  )
)