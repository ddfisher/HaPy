from Tkinter import *
from HaPy import Calc

LINEBEGIN = "lb"

class App:
    def __init__(self, master):
        frame = Frame(master)
        frame.pack()

        self.button = Button(frame, text="QUIT", fg="red", command=frame.quit)
        self.button.pack(side=LEFT)

        self.button = Button(frame, text="Evaluate", command=self.evaluate)
        self.button.pack(side=LEFT)

        self.inputBox = Text(frame)
        self.inputBox.pack(side=LEFT)
        self.inputBox.bind("<KeyPress>", self.onkeypress)
        self.inputBox.mark_set(LINEBEGIN, INSERT)
        self.inputBox.mark_gravity(LINEBEGIN, LEFT)

        self.outputBox = Text(frame)
        self.outputBox.pack(side=BOTTOM)

    def onkeypress(self, e):
        if e.keycode == 36:
            self.evaluate()

    def evaluate(self):
        expr = (self.inputBox.get(LINEBEGIN, INSERT)).strip()
        exprAscii = expr.encode("utf8", "ignore")
        self.inputBox.mark_set(LINEBEGIN, INSERT)
        result = Calc.evaluate(exprAscii)
        self.outputBox.insert(END, result)
        self.outputBox.insert(END, "\n")


root = Tk()
app = App(root)
root.mainloop()
