package api;

import com.ericsson.otp.erlang.*;
import utils.Position;

import java.io.IOException;

public class ErlangApiService {
    private final OtpConnection connection;
    private final String genServer = "chompApi_gen_server";

    public ErlangApiService(String cookie, String serverNodeName) throws IOException, OtpAuthException {
        var client = new OtpSelf("chompClientNode", cookie);
        var server = new OtpPeer(serverNodeName);
        this.connection = client.connect(server);
    }

    public void startNewGame() throws IOException, OtpAuthException, OtpErlangExit {
        connection.sendRPC(genServer, "newGame", new OtpErlangList());
        connection.receiveRPC();
    }

    public OtpErlangObject getGame() throws IOException, OtpAuthException, OtpErlangExit {
        connection.sendRPC(genServer, "getGame", new OtpErlangList());
        return connection.receiveRPC();
    }

    public OtpErlangObject sendMove(Position position) throws IOException, OtpAuthException, OtpErlangExit {
        OtpErlangObject[] positionObject = {new OtpErlangInt(position.x()), new OtpErlangInt(position.y())};
        var tuple = new OtpErlangTuple(positionObject);
        var positionList = new OtpErlangList(tuple);
        connection.sendRPC(genServer, "takeTurn", positionList);
        return connection.receiveRPC();
    }
}
