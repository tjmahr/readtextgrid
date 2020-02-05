Read from file: "draw-tg-parts.Collection"

Erase all
Black
selectObject: "TextGrid left-marginal-text"
Select outer viewport: 1, 2, 0, 4.5
Draw: 0, 0, "no", "yes", "no"
White
selectObject: "TextGrid left-marginal-text-mask"
Draw: 0, 0, "no", "yes", "no"
Black

Select outer viewport: 5.25, 7, 0, 4.5
selectObject: "TextGrid right-marginal-text"
Draw: 0, 0, "no", "yes", "no"
White
selectObject: "TextGrid right-marginal-text-mask"
Draw: 0, 0, "no", "yes", "no"
Black

Select outer viewport: 1, 6.5, 0, 4.5
selectObject: "TextGrid Mary_John_bell"
Draw: 0, 0, "no", "yes", "no"
Select outer viewport: 1, 6.5, 3.5, 5.5
selectObject: "TextGrid blue-text"
Blue
Draw: 0, 0, "no", "yes", "no"
selectObject: "TextGrid blue-text-mask"
White
Draw: 0, 0, "no", "yes", "no"
selectObject: "TextGrid forehead-and-chin"
Black
Draw: 0, 0, "no", "yes", "no"

Select outer viewport: 1, 6.5, 0, 2
selectObject: "TextGrid forehead-and-chin"
Black
Draw: 0, 0, "no", "yes", "no"

Select outer viewport: 1, 7, 0, 5.5
Save as 300-dpi PNG file: "tg-parts.png"

