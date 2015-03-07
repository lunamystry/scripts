import setuptools
import distutils.cmd
import distutils.log

class Roger(distutils.cmd.Command):
    """a custom command that is written to maybe help in setup you see """
    description = __doc__
    user_options = []

    def initialize_options(self):
        """ Pre-process options """
        pass

    def finalize_options(self):
        """ Post-process options """
        pass

    def run(self):
        """ Run command """
        print("shrubberies are my trade.")
        print("I am a shrubber.")
        print("My name is Roger the shrubber.")
        print("I arrange, design and sell Shrubberies.")


setuptools.setup(
        name='cypher',
        version='0.1',
        description='As I learn Cypher basics',
        author='Leonard Mbuli',
        author_email='leonard@praekelt.com',
        license='BSD',
        py_modules=['ceaser'],
        packages=setuptools.find_packages(),
        install_requires=[
            'Click>=3.3',
            'pylint>=1.4.1'
            ],
        cmdclass = {
            'roger': Roger
            }
)


