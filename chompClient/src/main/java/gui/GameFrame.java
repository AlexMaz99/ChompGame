package gui;

import javafx.application.Application;
import javafx.application.Platform;
import javafx.scene.Scene;
import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.effect.DropShadow;
import javafx.scene.layout.Pane;
import javafx.scene.paint.Color;
import javafx.stage.Stage;
import utils.Position;

import java.util.ArrayList;
import java.util.List;

public class GameFrame extends Application {
    private final int cellSize = 80;
    private final int cellGap = 5;
    private final int x = 5 * cellSize - cellGap;
    private final int y = 4 * cellSize - cellGap;
    private final List<Position> cellPositions = new ArrayList<>();

    public static void main(String[] args) {
        launch(args);
    }

    @Override
    public void start(Stage stage) {
        for (int i = 0; i < 5; i++) {
            for (int j = 0; j < 4; j++) {
                cellPositions.add(new Position(i, j));
            }
        }
        stage.setTitle("ChompGame");
        stage.setResizable(false);
        stage.setOnCloseRequest(event -> Platform.exit());

        var root = new Pane();
        var canvas = new Canvas(x, y);
        root.getChildren().add(canvas);
        var graphics = canvas.getGraphicsContext2D();
        buildScene(graphics);

        var scene = new Scene(root, Color.SIENNA);
        stage.setScene(scene);
        stage.show();

    }

    private void buildScene(GraphicsContext graphics) {
        graphics.setFill(Color.SADDLEBROWN);
        var dropShadow = new DropShadow();
        dropShadow.setRadius(5.0);
        dropShadow.setOffsetX(3.0);
        dropShadow.setOffsetY(3.0);
        dropShadow.setColor(Color.BLACK);
        graphics.setEffect(dropShadow);
        cellPositions.forEach(position -> graphics.fillRect(position.x() * cellSize, position.y() * cellSize, cellSize - cellGap, cellSize - cellGap));
    }
}
