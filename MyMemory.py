from enum import Enum

class CallStack:
    def __init__(self):
        self._records = []

    def push(self, ar):
        self._records.append(ar)

    def pop(self):
        self._records.pop()

    def top(self):
        return self._records[-1]

    def get_ID(self, ID):
        # this method iterates through the call stack and returns the first occurence of ID
        for ar in reversed(self._records):
            if ar.get(ID) is not None:
                return ar.get(ID)
        return None

    def set_ID(self, ID, val):
        # this method iterates through the call stack and sets the first occurence of ID to val
        # if nothing is found it creates the ID
        for ar in reversed(self._records):
            if ar.get(ID) is not None:
                ar[ID] = val
                return
        self.top()[ID] = val

    def __str__(self):
        s = '\n'.join(repr(ar) for ar in reversed(self._records))
        s = f'CALL STACK\n{s}\n'
        return s

    def __repr__(self):
        return self.__str__()


class ARType(Enum):
    PROGRAM = 'PROGRAM'
    PROCEDURE = 'PROCEDURE'


class ActivationRecord:
    def __init__(self, name, type, nesting_level):
        self.name = name
        self.type = type
        self.nesting_level = nesting_level
        self.members = {}

    def __setitem__(self, key, value):
        self.members[key] = value

    def __getitem__(self, key):
        return self.members[key]

    def get(self, key):
        return self.members.get(key)

    def __str__(self):
        lines = [
            '{level}: {type} {name}'.format(
                level=self.nesting_level,
                type=self.type.value,
                name=self.name,
            )
        ]
        for name, val in self.members.items():
            lines.append(f'   {name:<20}: {val}')

        s = '\n'.join(lines)
        return s

    def __repr__(self):
        return self.__str__()
