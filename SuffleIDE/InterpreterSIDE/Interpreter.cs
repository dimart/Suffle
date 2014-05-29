using Microsoft.Practices.Prism.Modularity;
using Microsoft.Practices.Unity;
using Side.Interfaces.Services;

namespace InterpreterSIDE
{
    [Module(ModuleName = "Side.Interpreter")]
    public class Interpreter : IModule
    {
        private IUnityContainer _container;

        public Interpreter(IUnityContainer container)
        {
            _container = container;
        }

        #region IModule Members

        public void Initialize()
        {
            _container.RegisterType<IInterpreter, Controller>(new ContainerControlledLifetimeManager());
        }

        #endregion
    }
}
