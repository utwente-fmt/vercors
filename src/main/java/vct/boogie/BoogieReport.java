// -*- tab-width:2 ; indent-tabs-mode:nil -*-
package vct.boogie;

import hre.ast.TrackingTree;
import hre.io.Message;
import hre.io.MessageProcess;

import java.io.File;
import java.io.IOException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import vct.util.Configuration;

import static hre.lang.System.*;

/**
 * Create a test report for a Boogie run from the output file.
 *
 * @author sccblom
 */
public class BoogieReport extends hre.util.TestReport {

    private boolean finished = false;
    private int error_count = 0;
    private int verified_count = 0;
    private int timeout_count = 0;

    private static Document parseXmlFile(File file) {
        //get the factory
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();

        try {

            //Using factory get an instance of document builder
            DocumentBuilder db = dbf.newDocumentBuilder();

            //parse using builder to get DOM representation of the XML file
            return db.parse(file);


        } catch (ParserConfigurationException pce) {
            DebugException(pce);
        } catch (SAXException se) {
            DebugException(se);
        } catch (IOException ioe) {
            DebugException(ioe);
        }
        System.exit(1);
        return null;
    }

    //public BoogieReport(InputStream stream,String filename,TrackingTree tree) throws IOException {
    public BoogieReport(String tool, MessageProcess shell, File boogie_xml_file, TrackingTree tree) throws IOException {
        String line;
        for (; ; ) {
            Message msg = shell.recv();
            if (msg.getFormat().equals("exit %d")) break;
            if (msg.getFormat().equals("stderr: %s") || msg.getFormat().equals("stdout: %s")) {
                line = (String) msg.getArg(0);
            } else {
                continue;
            }
            if (line.matches(".*" + tool + " program verifier finished.*")) {
                finished = true;
                String words[] = line.split(" ");
                for (int i = 1; i < words.length; i++) {
                    if (words[i].matches("verified.*")) verified_count = Integer.parseInt(words[i - 1]);
                    if (words[i].matches("error.*")) error_count = Integer.parseInt(words[i - 1]);
                    if (words[i].equals("time")) timeout_count = Integer.parseInt(words[i - 1]);
                }
            }
            Warning("unclaimed output in Boogie report: %s", line);
        }
        if (!finished) {
            setVerdict(Verdict.Error);
        } else if (timeout_count > 0) {
            setVerdict(Verdict.Inconclusive);
        } else if (error_count > 0) {
            setVerdict(Verdict.Fail);
        } else {
            setVerdict(Verdict.Pass);
        }
        Progress("parsing %s", boogie_xml_file);
        Document dom = parseXmlFile(boogie_xml_file);
        Progress("interpreting %s", boogie_xml_file);
        parseDocument(dom, tree);
        Progress("finished %s", boogie_xml_file);
    }

    private void parseDocument(Document dom, TrackingTree tree) {
        //get the root elememt
        Element docEle = dom.getDocumentElement();

        //get a nodelist
        NodeList method_list = docEle.getElementsByTagName("method");
        for (int i = 0; i < method_list.getLength(); i++) {
            Element method = (Element) method_list.item(i);
            NodeList error_list = method.getElementsByTagName("error");
            if (!Configuration.detailed_errors.get()) {
                if (error_list.getLength() > 0) {
                    Element error = (Element) error_list.item(0);
                    String message = error.getAttribute("message");
                    Output("method %s: Fail (%s)", method.getAttribute("name"), message);
                } else {
                    Output("method %s: Pass", method.getAttribute("name"));
                }
            } else {
                Output("method %s: %d error(s)", method.getAttribute("name"), error_list.getLength());
                for (int j = 0; j < error_list.getLength(); j++) {
                    Element error = (Element) error_list.item(j);
                    String message = error.getAttribute("message");
                    int line = Integer.parseInt(error.getAttribute("line"));
                    int column = Integer.parseInt(error.getAttribute("column"));
                    Output("  %s at %s", message, tree.getOrigin(line, column));
                    NodeList related_list = error.getElementsByTagName("related");
                    for (int k = 0; k < related_list.getLength(); k++) {
                        Element related = (Element) related_list.item(k);
                        line = Integer.parseInt(related.getAttribute("line"));
                        column = Integer.parseInt(related.getAttribute("column"));
                        Output("  see also %s", tree.getOrigin(line, column));
                    }
                    NodeList trace_list = error.getElementsByTagName("trace");
                    if (trace_list.getLength() > 1) throw new Error("more than one trace");
                    if (trace_list.getLength() == 0) throw new Error("missing trace");
                    Output("    trace is:");
                    NodeList step_list = error.getElementsByTagName("traceNode");
                    for (int k = 0; k < step_list.getLength(); k++) {
                        Element step = (Element) step_list.item(k);
                        String attr = step.getAttribute("line");
                        if (attr != null && !attr.equals("")) {
                            line = Integer.parseInt(attr);
                            column = Integer.parseInt(step.getAttribute("column"));
                            Output("      -> %s", tree.getOrigin(line, column));
                        }
                    }
                }
            }
        }
    }

    public boolean getFinished() {
        return finished;
    }

    public int getVerified() {
        return verified_count;
    }

    public int getErrors() {
        return error_count;
    }

    public int getTimeouts() {
        return timeout_count;
    }

}

