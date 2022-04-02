from pyrlang.node import Node
from pyrlang.process import Process
from term import Atom

def main():
    node = Node(node_name="pynode@127.0.0.1", cookie="testingpyrlang123")
    event_loop = node.get_loop()

    listener = Listener()

    event_loop.call_soon(lambda: node.send_nowait(
        sender=listener.pid_,
        receiver=(Atom("erlnode@127.0.0.1"), Atom("shell")),
        message=Atom("Sending a message from python to erlang"),
    ))

    node.run()

class Listener(Process):
    def __init__(self) -> None:
        Process.__init__(self)
        self.get_node().register_name(self, Atom("listener"))

    def handle_one_inbox_message(self, msg):

        if type(msg) == str:
            print(ord(msg[0]), end="")
            for item in msg[1:]:
                print(f", {ord(item)}", end="")
            print()
        elif msg == Atom("quit"):
            self.exit()
        else:
            print(msg)

if __name__ == "__main__":
    main()
