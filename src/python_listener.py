
import threading, queue
from erlastic import port_connection, encode, Atom as A
from pprint import pprint

def main():

    # Messages go into mailbox, can send encoded terms to port
    mailbox, port = port_connection()

    q = []

    # equivalent of recieve loop in erl
    for (tag, data) in mailbox:
        if tag == A("init_state"):
            set_initial_state(data)
        elif tag == A("update"):
            update_car(data, q)
            if len(q) > 100:
                port.send(encode(A("please stop"), 5))
                dump_queue(q)

def set_initial_state(data: tuple) -> None:
    with open("output.txt", "a", encoding="utf-8") as f:
        print(f"got a state: {data}", file=f)

def update_car(update: tuple, work_queue: list) -> None:
    work_queue.append(update)

def dump_queue(jobs: list) -> None:
    with open("output.txt", "a", encoding="utf-8") as f:
        for job in jobs:
            print(f"crisis alert! {job}", file=f, flush=True)
    jobs.clear()

if __name__ == "__main__":
    main()
