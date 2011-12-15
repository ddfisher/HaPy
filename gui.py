from Tkinter import *
from HaPy import Newton

class App:
    def __init__(self, master):
        frame = Frame(master)
        frame.pack()

        self.button = Button(frame, text="QUIT", fg="red", command=frame.quit)
        self.button.pack(side=LEFT)

        self.button = Button(frame, text="Evaluate", command=self.evaluate)
        self.button.pack(side=LEFT)


        self.inputBox = Text(frame)
        self.inputBox.insert(END, "hi")
        self.inputBox.pack(side=LEFT)

    def evaluate(self):
        poly = Newton.parsePolynomial("x + 1")
        print Newton.evaluate(poly, 1.0)

root = Tk()
app = App(root)
root.mainloop()
