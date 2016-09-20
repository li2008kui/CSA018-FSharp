namespace ThisCoder.CSA018

module MessageType =
    type MessageType =
        | Command = 0x01
        | CommandACK = 0x02
        | Event = 0x03
        | EventACK = 0x04
        | CommandResult = 0x05
        | HeartbeatData = 0xFF
        | HeartbeatResponse = 0xFE