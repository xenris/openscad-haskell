module Model where

import OpenSCAD

baseWidth = 120
baseDepth = 80
baseHeight = 15
baseX = 0
baseY = 0
baseZ = baseHeight / 2

baseSlotWidth = 60
baseSlotDepth = baseDepth
baseSlotHeight = 5
baseSlotX = baseX
baseSlotY = baseY
baseSlotZ = (baseZ - baseHeight / 2) + (baseSlotHeight / 2)

baseHoleHeight = baseHeight
baseHoleDiameter = 10
baseHoleX = baseX + (90 / 2)
baseHoleY = baseY + (50 / 2)
baseHoleZ = (baseZ - baseHeight / 2) + (baseHoleHeight / 2)

hingeWidth = 70
hingeDepth = 60
hingeHeight = hingeWidth
hingeX = baseX + (baseWidth / 2) - 30
hingeY = baseY
hingeZ = baseZ + baseHeight + 60

main = writeFile "model.scad" (render model 50)

model = base +. hinge

base = block -. slot -. holes
    where
        block = (cube `scale` (v3 baseWidth baseDepth baseHeight) `translate` (v3 baseX baseY baseZ))
        slot = (cube `scale` (v3 baseSlotWidth baseSlotDepth baseSlotHeight) `translate` (v3 baseSlotX baseSlotY baseSlotZ))
        holes = union $ map hole [(1, 1), (1, -1), (-1, -1), (-1, 1)]
        hole (x, y) = cylinder `scale` (v3 baseHoleDiameter baseHoleDiameter baseHoleHeight) `translate` (v3 (baseHoleX * x) (baseHoleY * y) baseHoleZ)

hinge = (cylinder `scale` (v3 70 70 60)) -. (cylinder `scale` (v3 50 50 60)) `rotate` (v1 pi / 2, (v3 1 0 0)) `translate` (v3 hingeX hingeY hingeZ)
