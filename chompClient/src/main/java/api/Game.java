package api;

import com.ericsson.otp.erlang.*;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleStringProperty;
import utils.Position;

import java.util.ArrayList;
import java.util.List;

public class Game {
    private final List<Position> eatenChocolates = new ArrayList<>();
    private final SimpleStringProperty information = new SimpleStringProperty("First player turn");
    private final SimpleBooleanProperty hasEnded = new SimpleBooleanProperty(false);

    public void restart() {
        eatenChocolates.clear();
        information.set("First player turn");
        hasEnded.set(false);
    }

    public void readReply(OtpErlangObject reply) {
        var replyTuple = (OtpErlangTuple) reply;

        readEatenChocolates(replyTuple.elementAt(1));
        readTurn(replyTuple.elementAt(2));
        readIfGameHasEnded(replyTuple.elementAt(3));

        if (hasEnded.get())
            setWhoWinInformation(replyTuple.elementAt(2));
    }

    private void readEatenChocolates(OtpErlangObject otpErlangObject) {
        if (!(otpErlangObject instanceof OtpErlangList) || otpErlangObject.equals(new OtpErlangList()))
            return;

        ((OtpErlangList) otpErlangObject).forEach(object -> {
            try {
                var tuple = (OtpErlangTuple) object;
                var x = ((OtpErlangLong) tuple.elementAt(0)).intValue();
                var y = ((OtpErlangLong) tuple.elementAt(1)).intValue();
                var position = new Position(x, y);
                if (!this.eatenChocolates.contains(position))
                    this.eatenChocolates.add(position);
            } catch (OtpErlangRangeException e) {
                e.printStackTrace();
            }
        });
    }

    private void readTurn(OtpErlangObject otpErlangObject) {
        var firstPlayerTurn = (OtpErlangAtom) otpErlangObject;
        this.information.set(firstPlayerTurn.booleanValue() ? "First player turn" : "Second player turn");
    }

    private void readIfGameHasEnded(OtpErlangObject otpErlangObject) {
        var hasEnded = (OtpErlangAtom) otpErlangObject;
        this.hasEnded.set(hasEnded.booleanValue());
    }

    private void setWhoWinInformation(OtpErlangObject otpErlangObject) {
        var firstPlayerTurn = (OtpErlangAtom) otpErlangObject;
        this.information.set(firstPlayerTurn.booleanValue() ? "Second player won" : "First player won");
    }

    public List<Position> getEatenChocolates() {
        return eatenChocolates;
    }

    public SimpleBooleanProperty hasEnded() {
        return hasEnded;
    }

    public SimpleStringProperty informationProperty() {
        return information;
    }
}
