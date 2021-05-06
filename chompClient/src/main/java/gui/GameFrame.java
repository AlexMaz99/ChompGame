package gui;

import api.ErlangApiService;
import api.Game;
import com.ericsson.otp.erlang.OtpAuthException;
import com.ericsson.otp.erlang.OtpErlangExit;
import javafx.application.Application;
import javafx.application.Platform;
import javafx.scene.Scene;
import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.control.Button;
import javafx.scene.effect.DropShadow;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.Pane;
import javafx.scene.paint.Color;
import javafx.scene.text.Text;
import javafx.stage.Stage;
import utils.Position;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class GameFrame extends Application {
    private ErlangApiService erlangApiService;
    private Game game;
    private final int cellSize = 80;
    private final int cellGap = 5;
    private final int width = 5 * cellSize - cellGap;
    private final int height = 4 * cellSize - cellGap;
    private final List<Position> cellPositions = new ArrayList<>();

    public static void main(String[] args) {
        launch(args);
    }

    @Override
    public void start(Stage stage) throws OtpAuthException, IOException, OtpErlangExit {
        this.erlangApiService = new ErlangApiService("erljava", "apiNode");
        this.erlangApiService.startNewGame();
        this.game = new Game();
        for (int i = 0; i < 5; i++) {
            for (int j = 0; j < 4; j++) {
                cellPositions.add(new Position(i, j));
            }
        }
        stage.setTitle("ChompGame");
        stage.setResizable(false);
        stage.setOnCloseRequest(event -> Platform.exit());

        var root = new Pane();
        var canvas = new Canvas(width, height + 100);
        root.getChildren().add(canvas);
        var graphics = canvas.getGraphicsContext2D();
        buildScene(graphics);
        buildFooter(graphics);

        var startNewGameButton = new Button("Start new game");
        startNewGameButton.setLayoutX(width / 2 - 45);
        startNewGameButton.setLayoutY(height + 60);
        startNewGameButton.setOnMouseClicked(mouseEvent -> starNewGame(graphics));
        startNewGameButton.visibleProperty().bind(game.hasEnded());
        root.getChildren().add(startNewGameButton);

        var turnText = new Text();
        turnText.setLayoutX(width / 2 - 45);
        turnText.setLayoutY(height + 40);
        turnText.textProperty().bind(game.informationProperty());
        root.getChildren().add(turnText);

        var scene = new Scene(root, Color.SIENNA);
        scene.addEventFilter(MouseEvent.MOUSE_CLICKED, mouseEvent -> handleClick(mouseEvent.getX(), mouseEvent.getY(), graphics));
        stage.setScene(scene);
        stage.show();
    }

    private void buildScene(GraphicsContext graphics) {
        graphics.clearRect(0, 0, width, height);

        graphics.setFill(Color.WHITESMOKE);
        graphics.fillRect(0, 0, width, height + 100);

        graphics.setFill(Color.SIENNA);
        graphics.fillRect(0, 0, width, height);

        graphics.setFill(Color.SADDLEBROWN);
        var dropShadow = new DropShadow();
        dropShadow.setRadius(5.0);
        dropShadow.setOffsetX(3.0);
        dropShadow.setOffsetY(3.0);
        dropShadow.setColor(Color.BLACK);
        graphics.setEffect(dropShadow);

        cellPositions.forEach(position -> graphics.fillRect(position.x() * cellSize, position.y() * cellSize, cellSize - cellGap, cellSize - cellGap));
    }

    private void buildFooter(GraphicsContext graphics) {
        graphics.setFill(Color.WHITESMOKE);
        graphics.fillRect(0, height + cellGap, width, 100 - cellGap);
    }

    private void handleClick(double clickedX, double clickedY, GraphicsContext graphics) {
        if (clickedX > width || clickedY > height) return;
        var x = (int) (clickedX / cellSize);
        var y = (int) (clickedY / cellSize);
        var position = new Position(x, y);

        try {
            var reply = this.erlangApiService.sendMove(position);
            game.readReply(reply);
            refreshScene(graphics);
        } catch (IOException | OtpAuthException | OtpErlangExit e) {
            e.printStackTrace();
        }
    }

    private void refreshScene(GraphicsContext graphics) {
        graphics.setFill(Color.BLACK);
        game.getEatenChocolates().forEach(position -> graphics.fillRect(position.x() * cellSize, position.y() * cellSize, cellSize, cellSize));
    }

    private void starNewGame(GraphicsContext graphics) {
        try {
            erlangApiService.startNewGame();
            game.restart();
            buildScene(graphics);
        } catch (IOException | OtpAuthException | OtpErlangExit e) {
            e.printStackTrace();
        }
    }
}
