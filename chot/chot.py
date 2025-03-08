import pytchat
import time
import sys
import socket

def stdout(id):
    chat = pytchat.create(video_id=id)
    print("## CHOTTING ##\n")
    while chat.is_alive():
        try:
            for c in chat.get().sync_items():
                print(f"<{c.author.name}> {c.message}")
            time.sleep(0.2)
        except KeyboardInterrupt:
            chat.terminate()
            break
    exit(0)

def ports(id, port, verbose=True):
    s = socket.socket()
    chat = pytchat.create(video_id=id)
    s.bind(('localhost', int(port)))
    s.listen()
    if (verbose):
        print("[chot] Ready")
    conn, addr = s.accept()
    conn.send("## CONNECTED TO CHOT ##\n\n".encode())
    if (verbose):
        print("[chot] connected to chot!")
    while chat.is_alive():
        try:
            for c in chat.get().sync_items():
                # print(f"[{c.datetime}] <{c.author.name}> {c.message}")
                conn.send(f"<{c.author.name}> {c.message}\n".encode())
                if (verbose):
                    print(f"<{c.author.name}> {c.message}")
            time.sleep(0.2)
        except KeyboardInterrupt:
            chat.terminate()
            break
    conn.close()
    exit(0)

if __name__=='__main__':
    if (len(sys.argv) == 2):
        stdout(sys.argv[1])
    else:
        ports(sys.argv[1], sys.argv[2], True if len(sys.argv) == 4 and sys.argv[3] == '-v' else False)
