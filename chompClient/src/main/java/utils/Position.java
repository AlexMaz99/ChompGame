package utils;

public record Position(int x, int y) {

    public String toString() {
        return "(" + this.x + "," + this.y + ")";
    }

    public boolean equals(Object other) {
        if (this == other) return true;
        if (!(other instanceof Position that)) return false;
        return this.x == that.x && this.y == that.y;
    }

    @Override
    public int hashCode() {
        int hash = 13;
        hash += this.x * 31;
        hash += this.y * 17;
        return hash;
    }
}
