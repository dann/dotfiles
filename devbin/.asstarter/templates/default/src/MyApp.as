package {
    import flash.display.Sprite;
    import flash.text.TextField;
    import flash.events.MouseEvent;
    import caurina.transitions.Tweener;
    import HelloWorld;

    public class MyApp extends Sprite {
        public function MyApp() {
            var hw:HelloWorld = new HelloWorld();
            var tf:TextField = new TextField();
            tf.text = "Hello Tweener";
            addChild(tf);
            stage.addEventListener(MouseEvent.CLICK, function(e:MouseEvent):void {
                Tweener.addTween(tf, {x:mouseX, y:mouseY, time:1, transition:"linear"});
            });
        }
    }
}

