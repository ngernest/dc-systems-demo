import Demo.PPM



def allBlue : PPM := Id.run do
  let mut pixels : Array RGB := Array.empty
  for _ in [0:200] do
    for _ in [0:200] do
      pixels := pixels.push ⟨0, 0, 255⟩
  return {width := 200, height := 200, pixels}

#ppm pure allBlue



def gradient : PPM := Id.run do
  let width : UInt64 := 300
  let height : UInt64 := 200
  let mut pixels : Array RGB := Array.empty
  for j in [0:height.toNat] do
    for i in [0:width.toNat] do
      pixels := pixels.push ⟨
          ((i.toFloat / width.toFloat) * 255.0).toUInt8,
          ((j.toFloat / height.toFloat) * 255.0).toUInt8,
          0,
        ⟩
  return {width, height, pixels}

#ppm pure gradient
