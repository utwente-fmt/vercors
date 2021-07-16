package hre.util;

import hre.io.Container;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import static hre.lang.System.*;

public class ContainerClassLoader extends ClassLoader {
    private final Container source;

    public ContainerClassLoader(Container source) throws IOException {
        super(ContainerClassLoader.class.getClassLoader()); //calls the parent class loader's constructor
        this.source = source;
    }

    protected synchronized Class<?> loadClass(String className, boolean resolve) throws ClassNotFoundException {
        Class<?> cls = findLoadedClass(className);
        if (cls != null) {
            return cls;
        }
        Debug("trying to load %s", className);
        String clsFile = className.replace('.', '/') + ".class";
        if (!source.contains(clsFile)) {
            Debug("using parent loader for %s", className);
            return super.loadClass(className, resolve);
        }
        Debug("using ContainerClassLoader for %s", className);
        InputStream is;
        try {
            is = source.read(clsFile);
        } catch (IOException e) {
            throw new ClassNotFoundException("IOException: " + e);
        }
        // 3. get bytes for class
        byte[] classBytes = new byte[(int) source.size(clsFile)];
        try {
            int read = 0;
            int expect = (int) source.size(clsFile);
            while (read < expect) {
                int tmp = is.read(classBytes, read, (expect - read));
                if (tmp == 0) Fail("not enough data");
                read += tmp;
            }
        } catch (IOException e) {
            Fail("ERROR loading class file: " + e);
        }
        // 4. turn the byte array into a Class
        try {
            cls = defineClass(className, classBytes, 0, classBytes.length);
            Package pkg = cls.getPackage();
            if (pkg == null) {
                Debug("class %s has no package", className);
                String pkgName = className.substring(0, className.lastIndexOf('.'));
                Debug("defining package %s", pkgName);
                String name = pkgName;
                String specTitle = "specTitle";
                String specVersion = "specVersion";
                String specVendor = "specVendor";
                String implTitle = "implTitle";
                String implVersion = "implVersion";
                String implVendor = "implVendor";
                URL sealBase = null;
                definePackage(name, specTitle, specVersion, specVendor, implTitle, implVersion, implVendor, sealBase);
            }
            if (resolve) {
                resolveClass(cls);
            }
        } catch (SecurityException e) {
            // loading core java classes such as java.lang.String
            // is prohibited, throws java.lang.SecurityException.
            // delegate to parent if not allowed to load class
            cls = super.loadClass(className, resolve);
        }
        return cls;
    }

    @Override
    protected String findLibrary(String libname) {
        String fullname = System.mapLibraryName(libname);
        return source.findFile(fullname);
    }

    @Override
    public InputStream getResourceAsStream(String name) {
        if (source.contains(name)) {
            try {
                Debug("resource %s found.", name);
                return source.read(name);
            } catch (IOException e) {
                DebugException(e);
                return null;
            }
        } else {
            Warning("resource %s not found");
            return null;
        }
    }
}

