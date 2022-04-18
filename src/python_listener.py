import threading, queue, sys
from erlastic import port_connection, encode, Atom as A
from pprint import pprint

def main():
    # Messages go into mailbox, can send encoded terms to port
    mailbox, port = port_connection()

    q = []

    # equivalent of recieve loop in erl
    for (tag, data) in mailbox:
        if tag == A("update"):
            update_car(data, q)
            if len(q) > 10:
                break
            port.send(A("go"))

    dump_queue(q)
    port.send(A("stop"))

def set_initial_state(data: tuple) -> None:
    with open("output.txt", "a", encoding="utf-8") as f:
        print(f"got a state: {data}", file=f)


def update_car(update: tuple, work_queue: list) -> None:
    work_queue.append(update)


def dump_queue(jobs: list) -> None:
    with open("output.txt", "a", encoding="utf-8") as f:
        for job in jobs:
            print(f"received message: {job}", file=f, flush=True)
    jobs.clear()


if __name__ == "__main__":
    main()
