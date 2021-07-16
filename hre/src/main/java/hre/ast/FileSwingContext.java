package hre.ast;

import javax.swing.*;
import javax.swing.text.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class FileSwingContext extends JPanel implements ActionListener {


    /**
     * serial version
     */
    private static final long serialVersionUID = -8488962903581879604L;

    protected JTextPane textPane;
    protected JScrollPane paneScrollPane;
    protected StyledDocument doc;

    public FileSwingContext() {
        setLayout(new BorderLayout());

        // Create a text pane.
        textPane = new JTextPane();
        doc = textPane.getStyledDocument();
        addStylesToDocument(doc);

        textPane.setEditable(false);

        DefaultCaret caret = (DefaultCaret) textPane.getCaret();
        caret.setUpdatePolicy(DefaultCaret.ALWAYS_UPDATE);

        paneScrollPane = new JScrollPane(textPane);
        paneScrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
        paneScrollPane.setPreferredSize(new Dimension(750, 750));
        paneScrollPane.setMinimumSize(new Dimension(10, 10));
        add(paneScrollPane);
    }

    public void actionPerformed(ActionEvent e) {

    }

    protected void addStylesToDocument(StyledDocument doc) {
        // Initialize some styles.
        Style def = StyleContext.getDefaultStyleContext().getStyle(
                StyleContext.DEFAULT_STYLE);

        Style regular = doc.addStyle("regular", def);
        StyleConstants.setFontFamily(def, "SansSerif");

        Style s = doc.addStyle("italic", regular);
        StyleConstants.setItalic(s, true);

        s = doc.addStyle("bold", regular);
        StyleConstants.setBold(s, true);

        s = doc.addStyle("small", regular);
        StyleConstants.setFontSize(s, 10);

        s = doc.addStyle("large", regular);
        StyleConstants.setFontSize(s, 16);

    }

}